open Internals
open Value

module type EQUAL_ASSERT = sig
  include VALUE_ASSERT

  val equal_to : t -> t assertion
end

module EqualAssertions (V : Values.EQ_VALUE) : EQUAL_ASSERT with type t = V.t = struct
  include ValueAssertions (V)

  let equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.equal expected actual)
           (Equality {expected_str = V.to_string expected; actual_str = V.to_string actual}) )
end
