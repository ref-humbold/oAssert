open Internals

module type LIST_ASSERT = sig
  type elem

  val empty : elem list assertion

  val of_length : int -> elem list assertion

  val equal_to : elem list -> elem list assertion

  val containing : elem -> elem list assertion

  val containing_all : elem list -> elem list assertion

  val containing_any : elem list -> elem list assertion

  val all_matching : (elem -> bool) -> elem list assertion

  val any_matching : (elem -> bool) -> elem list assertion
end
