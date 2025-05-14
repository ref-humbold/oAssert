open Internals.Assert

let true_ =
  Assertion
    (fun actual ->
       { is_success = actual;
         message = Equality {expected_str = string_of_bool true; actual_str = string_of_bool actual}
       } )

let false_ =
  Assertion
    (fun actual ->
       { is_success = not actual;
         message = Equality {expected_str = string_of_bool true; actual_str = string_of_bool actual}
       } )
