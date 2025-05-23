open Internals
open Type_assert

let true_ =
  Assertion
    (fun actual ->
       build_assertion
         actual
         (Equality
            {expected_str = string_of_bool true; actual_str = string_of_bool actual; negated = false}
         ) )

let false_ =
  Assertion
    (fun actual ->
       build_assertion
         (not actual)
         (Equality
            {expected_str = string_of_bool false; actual_str = string_of_bool actual; negated = false}
         ) )

module Type (T : TYPE) : TYPE_ASSERT with type t = T.t = struct
  type t = T.t

  let equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (expected = actual)
           (Equality
              {expected_str = T.to_string expected; actual_str = T.to_string actual; negated = false}
           ) )
end
