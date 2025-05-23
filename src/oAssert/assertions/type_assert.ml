open Internals

module type TYPE = Type.TYPE

module type TYPE_ASSERT = sig
  type t

  val equal_to : t -> t assertion
end
