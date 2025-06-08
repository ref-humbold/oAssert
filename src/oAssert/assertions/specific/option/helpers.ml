open Internals

module type OPTION_ASSERT = sig
  type elem

  val none : elem option assertion

  val some : elem -> elem option assertion
end
