module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

val convert_to_html : string -> string

val find_by_username: string -> (module DB) -> (int * string * string) option Lwt.t Lwt.t
val create: string -> string -> (module DB) -> unit Lwt.t Lwt.t
val list_comments: (module DB) -> (int * string) list Lwt.t
val add_comment: string -> (module DB) -> unit Lwt.t

val rubicel: string -> string Lwt.t
