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

let () =
  Dream.run
    @@ Dream.logger
    @@ count_requests
    @@ stats_requests
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
        let word = Dream.param request "word" in
        let html_content = Page.render word in
        Dream.html html_content);

    Dream.get "templates/:word"
    (fun request ->
      Dream.param request "word"
      |> Templates.render_html
      |> Dream.html);
  ]
