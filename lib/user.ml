
module User = struct
  type t = {
    id : int;
    username : string;
    password_hash : string;
  }
end
