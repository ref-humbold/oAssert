open Internals

module ValueOf (V : Values.EQUATABLE_VALUE) : Type_assert.EQUATABLE_TYPE_ASSERT with type t = V.t =
struct
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
