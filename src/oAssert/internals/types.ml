module type TYPE = sig
  type t

  val to_string : t -> string
end
