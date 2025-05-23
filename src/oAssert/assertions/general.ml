open Internals

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
