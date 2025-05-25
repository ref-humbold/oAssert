open Internals

module type EQUATABLE_TYPE_ASSERT = sig
  type t

  val equal_to : t -> t assertion
end

module type COMPARABLE_TYPE_ASSERT = sig
  include EQUATABLE_TYPE_ASSERT

  val greater_than : t -> t assertion

  val greater_than_or_equal_to : t -> t assertion

  val less_than : t -> t assertion

  val less_than_or_equal_to : t -> t assertion
end
