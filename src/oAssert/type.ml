module type TYPE = sig
  type t

  val to_string : t -> string
end

module type EQUATABLE_TYPE = sig
  include TYPE

  val equal : t -> t -> bool
end

module Int : EQUATABLE_TYPE with type t = int = Stdlib.Int

module Float : EQUATABLE_TYPE with type t = float = Stdlib.Float

module String : EQUATABLE_TYPE with type t = string = struct
  include Stdlib.String

  let to_string s = Printf.sprintf "\"%s\"" @@ Stdlib.String.escaped s
end
