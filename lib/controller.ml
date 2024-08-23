module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

open Caqti_request.Infix
open Caqti_type.Std

let convert_to_html param =
  Printf.sprintf "<html><body><h1>%s!</h1></body></html>" (Dream.html_escape param)

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

let find_by_username =
  let query =
    (T.string ->? T.(tup4 int string string string))
    "SELECT id, username, password_hash, email FROM users WHERE username = $1"
  in
  fun username (module Db: DB) ->
    let%lwt user_or_error = Db.find_opt query username in
    (Caqti_lwt.or_fail user_or_error)

(* SQL query to create a new user *)
let create =
  let query =
    (T.tup3 string string string ->. T.unit)
    "INSERT INTO users (username, password_hash, email) VALUES ($1, $2, $3)"
  in
  fun username password email (module Db: DB) ->
    let password_hash = Bcrypt.hash password |> Bcrypt.string_of_hash in
    let%lwt unit_or_error = Db.exec query (username, password_hash, email) in
    (Caqti_lwt.or_fail unit_or_error)

  let find_in_form name form =
    List.assoc_opt name form

  let signin_handler request =
    let%lwt form = Dream.form request in
    match form with
    | `Ok form ->
      (* Extract username and password from the form *)
      (match find_in_form "username" form, find_in_form "password" form with
        | Some username, Some password ->
          (* Here you would use your database logic *)
          let%lwt result = Dream.sql request (fun (module Db: DB) ->
            find_by_username username (module Db)
          ) in
            let%lwt response = match result with
            | Some (_id, _username, password_hash, _email) ->
              if Bcrypt.verify password (Bcrypt.hash_of_string password_hash) then
                (* Successful login *)
                let message = "Login successful" in
                let content = Components.Login.render_form ~message request in
                Lwt.return (Dream.respond ~status:`OK ~headers:[("Content-Type", "text/html")] content)
              else
                (* Incorrect password *)
                Lwt.return (Dream.respond ~status:`Bad_Request "Invalid username or password")
            | None ->
              (* User not found *)
              Lwt.return (Dream.respond ~status:`Bad_Request "User not found")
          in
          response
        | _ ->
          (* Missing username or password *)
          (Dream.respond ~status:`Bad_Request "Missing username or password"))
    | _->
      Dream.respond ~status:`Unauthorized "Form expired"


(* Define the signup handler *)
let signup_handler request : Dream.response Lwt.t =
  let%lwt form = Dream.form request in
  match form with
  | `Ok form ->
    (* Extract username and password from the form *)
    (match find_in_form "username" form, find_in_form "password" form, find_in_form "email" form with
     | Some username, Some password, Some email ->
       (* Check if username or password is empty *)
       if String.trim username = "" || String.trim password = "" || String.trim email = "" then
         Dream.respond ~status:`Bad_Request "Username and password must not be empty"
       else
         (* Insert the new user into the database *)
         let%lwt _result = Dream.sql request (fun (module Db : Caqti_lwt.CONNECTION) ->
           create username password email (module Db)
         ) in
        let message = "Signup successful" in
        let content = Components.Register.render_form ~message request in
        Dream.respond ~status:`OK ~headers:[("Content-Type", "text/html")] content

     | _ ->
       (* Missing username or password *)
       Dream.respond ~status:`Bad_Request "Username and password are required")
  | _ ->
    Dream.respond ~status:`Bad_Request "Form data is in the wrong format"
