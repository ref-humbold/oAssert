open Internals

module type EQUATABLE_TYPE_ASSERT = sig
  type t

  val equal_to : t -> t assertion
end
