open Common

module type TYPE = sig
  type t

  val to_string : t -> string
end

module type TYPE_ASSERT = sig
  type t

  val equal_to : t -> t assertion
end
