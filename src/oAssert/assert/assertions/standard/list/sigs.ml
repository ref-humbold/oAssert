open Internals
open Shared.Length

module type LIST_ASSERT = sig
  type elem

  module Length : LENGTH_ASSERT with type collection = elem list

  val empty : elem list assertion

  val equal_to : elem list -> elem list assertion

  val containing : elem -> elem list assertion

  val containing_all : elem list -> elem list assertion

  val containing_any : elem list -> elem list assertion

  val all_matching : (elem -> bool) -> elem list assertion

  val any_matching : (elem -> bool) -> elem list assertion
end
