open! Core_kernel
open! Async_kernel
open! Js_of_ocaml

module Html = Dom_html

let cascading_text ~print_text_every ~in_div text =
  Deferred.repeat_until_finished 0 (fun length ->
    if length > String.length text
    then return (`Finished ())
    else (
      in_div##.innerHTML := Js.string (String.prefix text length);
      let%map () = after print_text_every in
      `Repeat (length + 1)))

let onload _ =
  let d = Html.document in
  let body =
    Js.Opt.get (d##getElementById (Js.string "js_of_ocaml")) (fun () -> assert false)
  in
  let textbox = Html.createTextarea d in
  textbox##.rows := 20;
  textbox##.cols := 80;
  let content = Html.createDiv d in
  content##.style##.border := Js.string "1px black dashed";
  content##.style##.padding := Js.string "5px";
  Dom.appendChild body content;
  Dom.appendChild body (Html.createBr d);
  Dom.appendChild body textbox;
  ignore (
    let%bind () =
      cascading_text
        ~print_text_every:(Time_ns.Span.of_sec 0.05) 
        ~in_div:content
        "This is cascading text! Type something below, then &lt;enter&gt;:"
    in
    let%bind str =
      Deferred.repeat_until_finished () (fun () ->
        let value = Js.to_string (textbox##.value) in
        match String.lfindi value ~f:(fun _ -> (=) '\n') with
        | None ->
          let%map () = after (Time_ns.Span.of_sec 0.01) in
          `Repeat ()
        | Some i -> return (`Finished (String.prefix value i)))
    in
    cascading_text
      ~print_text_every:(Time_ns.Span.of_sec 0.05) 
      ~in_div:content
      ("You typed: " ^ str));
  Js._false

let _ =
  Async_js.init ();
  Html.window##.onload := Html.handler onload