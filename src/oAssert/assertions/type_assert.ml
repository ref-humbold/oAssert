open Internals

module type EQ_TYPE_ASSERT = sig
  type t

  val equal_to : t -> t assertion
end

module type CMP_TYPE_ASSERT = sig
  include EQ_TYPE_ASSERT

  val greater_than : t -> t assertion

  val greater_than_or_equal_to : t -> t assertion

  val less_than : t -> t assertion

  val less_than_or_equal_to : t -> t assertion
end
