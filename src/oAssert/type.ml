module type TYPE = sig
  type t

  val to_string : t -> string
end

module Int : TYPE with type t = int = struct
  type t = int

  let to_string = string_of_int
end

module Float : TYPE with type t = float = struct
  type t = float

  let to_string = string_of_float
end

module String : TYPE with type t = string = struct
  open Stdlib.String

  type t = string

  let to_string s = Printf.sprintf "\"%s\"" @@ escaped s
end
