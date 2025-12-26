open Internals
open Equal

module type COMPARE_ASSERT = sig
  include EQUAL_ASSERT

  val greater_than : t -> t assertion

  val greater_than_or_equal_to : t -> t assertion

  val less_than : t -> t assertion

  val less_than_or_equal_to : t -> t assertion
end

module CompareAssertions (V : Values.CMP_VALUE) : COMPARE_ASSERT with type t = V.t = struct
  include EqualAssertions (V)

  let greater_than expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected > 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be greater than %s" (V.to_string expected) } ) )

  let greater_than_or_equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected >= 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be greater than or equal to %s" (V.to_string expected)
              } ) )

  let less_than expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected < 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be less than %s" (V.to_string expected) } ) )

  let less_than_or_equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected <= 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be less than or equal to %s" (V.to_string expected) }
           ) )
end
