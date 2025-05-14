open Internals.Common
module I = Stdlib.Int

let zero =
  Assertion
    (fun actual ->
       { is_success = actual = 0;
         message = Equality {expected_str = "zero"; actual_str = string_of_int actual} } )

let positive =
  Assertion
    (fun actual ->
       { is_success = actual > 0;
         message = Condition {actual_str = string_of_int actual; description = "be positive"} } )

let negative =
  Assertion
    (fun actual ->
       { is_success = actual < 0;
         message = Condition {actual_str = string_of_int actual; description = "be negative"} } )

let equal_to expected =
  Assertion
    (fun actual ->
       { is_success = expected = actual;
         message = Equality {expected_str = string_of_int expected; actual_str = string_of_int actual}
       } )

let greater_than expected =
  Assertion
    (fun actual ->
       { is_success = actual > expected;
         message =
           Condition
             { actual_str = string_of_int actual;
               description = Printf.sprintf "be greater than %d" expected } } )

let greater_than_or_equal_to expected =
  Assertion
    (fun actual ->
       { is_success = actual >= expected;
         message =
           Condition
             { actual_str = string_of_int actual;
               description = Printf.sprintf "be greater than or equal to %d" expected } } )

let less_than expected =
  Assertion
    (fun actual ->
       { is_success = actual < expected;
         message =
           Condition
             { actual_str = string_of_int actual;
               description = Printf.sprintf "be less than %d" expected } } )

let less_than_or_equal_to expected =
  Assertion
    (fun actual ->
       { is_success = actual <= expected;
         message =
           Condition
             { actual_str = string_of_int actual;
               description = Printf.sprintf "be less than or equal to %d" expected } } )

let between start_inclusive end_inclusive =
  Assertion
    (fun actual ->
       { is_success = start_inclusive <= actual && actual <= end_inclusive;
         message =
           Condition
             { actual_str = string_of_int actual;
               description = Printf.sprintf "be between %d and %d" start_inclusive end_inclusive } } )

let strictly_between start_exclusive end_exclusive =
  Assertion
    (fun actual ->
       { is_success = start_exclusive < actual && actual < end_exclusive;
         message =
           Condition
             { actual_str = string_of_int actual;
               description =
                 Printf.sprintf "be strictly between %d and %d" start_exclusive end_exclusive } } )
