let convert_to_html param =
  Printf.sprintf "<html><body><h1>%s!</h1></body></html>" (Dream.html_escape param)

module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

let list_comments =
  let query =
    let open Caqti_request.Infix in
    (T.unit ->* T.(tup2 int string))
    "SELECT id, text FROM comment" in
  fun (module Db : DB) ->
    let%lwt comments_or_error = Db.collect_list query () in
    Caqti_lwt.or_fail comments_or_error

  let add_comment =
    let query =
      let open Caqti_request.Infix in
      (T.string ->. T.unit)
      "INSERT INTO comment (text) VALUES ($1)" in
    fun text (module Db : DB) ->
      let%lwt unit_or_error = Db.exec query text in
      Caqti_lwt.or_fail unit_or_error

(* Escape double quotes and backslashes in Ruby code *)
let escape_ruby_code code =
  String.to_seq code
  |> Seq.fold_left (fun acc c ->
     let escaped_char =
       match c with
       | '"' -> "\\\""
       | '\\' -> "\\\\"
       | _ -> String.make 1 c
     in
     acc ^ escaped_char
  ) ""

let rubicel code =
  (* Escape the Ruby code to be safe for command line execution *)
  let escaped_code = escape_ruby_code code in
  let command = Printf.sprintf "ruby -e \"%s\"" escaped_code in
  Lwt_process.pread ("", [| "/bin/sh"; "-c"; command |])
