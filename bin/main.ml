let count = ref 0
let successful = ref 0
let failed = ref 0
let count_requests inner_handler request =
  count := !count + 1;
  inner_handler request

let stats_requests inner_handler request =
  try%lwt
    let%lwt response = inner_handler request in
    successful := !successful + 1;
    Lwt.return response

  with exn ->
    failed := !failed + 1;
    raise exn

let prod_error _error debug_info suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in

  Dream.set_header suggested_response "Content-Type" Dream.text_html;
  Dream.set_body suggested_response begin
    Errors.render_error code debug_info reason
  end;
  Lwt.return suggested_response

let () =
    Dream.run ?error_handler:(match Sys.getenv_opt "DREAM_ENV" with
    | Some "dev" -> Some Dream.debug_error_handler
    | _ -> Some (Dream.error_template prod_error))
    @@ Dream.logger
    @@ count_requests
    @@ stats_requests
    @@ Dream.set_secret (match Sys.getenv_opt "COOKIE_SECRET" with | Some cookie_secret -> cookie_secret | _ ->  "foo")
    @@ Dream.memory_sessions
    @@ Dream.router [
      Dream.get "/"
        (fun _ ->
          Dream.html "Good morning, world!");

      Dream.get "/echo/:word"
        (fun request ->
          Dream.html (Dream.param request "word"));


      Dream.get "/count"
        (fun _ ->
          Dream.html (Printf.sprintf "Saw %i request(s)!" !count));


      Dream.get "/stats"
      (fun _ ->
        Dream.html (Printf.sprintf
        "%3i request(s) successful<br>%3i request(s) failed"!successful !failed));

      Dream.get "/fail"
      (fun _ ->
        Dream.warning (fun log -> log "Raising an exception!");
        raise (Failure "The Web app failed!"));

      Dream.get "/bad"
      (fun _ ->
        Dream.empty `Bad_Request);

      Dream.post "/echo" (fun request ->
      let%lwt body = Dream.body request in
        Dream.respond
          ~headers:["Content-Type", "application/octet-stream"]
          body);

      Dream.post "/echo_json" (fun request ->
        (match Dream.header request "Content-Type" with
        | Some "application/json" ->
            let%lwt body = Dream.body request in
            Dream.respond
              ~headers:["Content-Type", "application/json"]
              body
        | Some other_type ->
            Dream.log "Content-Type header %s recieved is not valid" other_type;
            Dream.respond ~status:`Unsupported_Media_Type "Unsupported Content-Type"
        | _ ->
            Dream.log "Content-Type header recieved is not recieved";
            Dream.respond ~status:`Bad_Request "Content-Type header should be application/json")
      );

      Dream.get "/page/:word" (fun request ->
        Dream.param request "word"
        |> fun word ->
        Page.render word
        |> Dream.html);

      Dream.get "templates/:word"
      (fun request ->
        Dream.param request "word"
        |> Templates.render_html
        |> Dream.html);

      Dream.get "login/:user"
      (fun request ->
        match Dream.session_field request "user" with
        | None ->
          let user = Dream.param request "user" in
          let%lwt () = Dream.invalidate_session request in
          let%lwt () = Dream.set_session_field request "user" user in
          Printf.ksprintf
            Dream.html "You weren't logged in; but now you are <b>%s!</b>" (Dream.html_escape user)

        | Some username ->
          Printf.ksprintf
            Dream.html "Welcome back, <b>%s!</b>" (Dream.html_escape username));

      Dream.get "/register/:name"
      (fun request ->
        match Dream.cookie request "dreaml.cookie.session" with
        | Some value ->
          Printf.ksprintf
            Dream.html "Your logged in name is %s!" (Dream.html_escape value)

        | None ->
          let username = Dream.param request "name" in
          let response = Dream.response "Set user name; come again!" in
          Dream.add_header response "Content-Type" Dream.text_html;
          Dream.set_cookie response request "dreaml.cookie.session" username;
          Lwt.return response);
  ]
