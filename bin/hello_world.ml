open! Core_kernel
open! Async_kernel
open! Js_of_ocaml

module Html = Dom_html

let onload _ =
  let d = Html.document in
  let body =
    Js.Opt.get (d##getElementById (Js.string "js_of_ocaml")) (fun () -> assert false)
  in
  let rec append_n_times n =
    if n < 1
    then Deferred.unit
    else
      let content = Html.createDiv d in
      content##.style##.border := Js.string "1px black dashed";
      content##.style##.padding := Js.string "5px";
      content##.innerHTML := Js.string (sprintf "%i!" n);
      Dom.appendChild body content;
      let%bind () = after (Time_ns.Span.of_sec 1.) in
      append_n_times (n - 1)
  in
  ignore (append_n_times 5);
  Js._false

let _ =
  Async_js.init ();
  Html.window##.onload := Html.handler onload