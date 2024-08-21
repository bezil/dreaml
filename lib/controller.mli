module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

val convert_to_html : string -> string

val find_by_username : string -> (module Caqti_lwt.CONNECTION) -> (int * string * string) option Lwt.t
val create: string -> string -> (module DB) -> unit Lwt.t
val find_in_form: 'a -> ('a * 'b) list -> 'b option
val signin_handler: Dream.request -> Dream.response Lwt.t
val signup_handler: Dream.request -> Dream.response Lwt.t

val list_comments: (module DB) -> (int * string) list Lwt.t
val add_comment: string -> (module DB) -> unit Lwt.t

val rubicel: string -> string Lwt.t
