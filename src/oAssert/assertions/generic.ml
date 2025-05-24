open Internals

module Type (T : Type.EQUATABLE_TYPE) : Type_assert.EQUATABLE_TYPE_ASSERT with type t = T.t = struct
  type t = T.t

  let equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (T.equal expected actual)
           (Equality
              {expected_str = T.to_string expected; actual_str = T.to_string actual; negated = false}
           ) )
end
