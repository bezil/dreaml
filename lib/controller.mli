module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

val convert_to_html : string -> string

val list_comments: (module DB) -> (int * string) list Lwt.t
val add_comment : string -> (module DB) -> unit Lwt.t
