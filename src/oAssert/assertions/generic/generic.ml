open Internals

module EqOf (V : Values.EQ_VALUE) : Helpers.EQ_TYPE_ASSERT with type t = V.t = struct
  type t = V.t

  let equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.equal expected actual)
           (Equality
              {expected_str = V.to_string expected; actual_str = V.to_string actual; negated = false}
           ) )
end

module CmpOf (V : Values.CMP_VALUE) : Helpers.CMP_TYPE_ASSERT with type t = V.t = struct
  include EqOf (V)

  let greater_than expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected > 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be greater than %s" (V.to_string expected);
                negated = false } ) )

  let greater_than_or_equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected >= 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be greater than or equal to %s" (V.to_string expected);
                negated = false } ) )

  let less_than expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected < 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be less than %s" (V.to_string expected);
                negated = false } ) )

  let less_than_or_equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected <= 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be less than or equal to %s" (V.to_string expected);
                negated = false } ) )
end
