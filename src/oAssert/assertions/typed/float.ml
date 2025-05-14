open Internals.Common
module F = Stdlib.Float

let nan =
  Assertion
    (fun actual ->
       { is_success = F.is_nan actual;
         message = Equality {expected_str = "NaN"; actual_str = string_of_float actual} } )

let zero =
  Assertion
    (fun actual ->
       { is_success = actual = 0.0;
         message = Equality {expected_str = "zero"; actual_str = string_of_float actual} } )

let positive =
  Assertion
    (fun actual ->
       { is_success = actual > 0.0;
         message = Condition {actual_str = string_of_float actual; description = "be positive"} } )

let negative =
  Assertion
    (fun actual ->
       { is_success = actual < 0.0;
         message = Condition {actual_str = string_of_float actual; description = "be negative"} } )

let close_to expected tolerance =
  Assertion
    (fun actual ->
       { is_success = expected -. tolerance <= actual && actual <= expected +. tolerance;
         message =
           Condition
             { actual_str = string_of_float actual;
               description = Printf.sprintf "be close to %f +/- %f" expected tolerance } } )

let greater_than expected =
  Assertion
    (fun actual ->
       { is_success = actual > expected;
         message =
           Condition
             { actual_str = string_of_float actual;
               description = Printf.sprintf "be greater than %f" expected } } )

let greater_than_or_equal_to expected =
  Assertion
    (fun actual ->
       { is_success = actual >= expected;
         message =
           Condition
             { actual_str = string_of_float actual;
               description = Printf.sprintf "be greater than or equal to %f" expected } } )

let less_than expected =
  Assertion
    (fun actual ->
       { is_success = actual < expected;
         message =
           Condition
             { actual_str = string_of_float actual;
               description = Printf.sprintf "be less than %f" expected } } )

let less_than_or_equal_to expected =
  Assertion
    (fun actual ->
       { is_success = actual <= expected;
         message =
           Condition
             { actual_str = string_of_float actual;
               description = Printf.sprintf "be less than or equal to %f" expected } } )

let between start_inclusive end_inclusive =
  Assertion
    (fun actual ->
       { is_success = start_inclusive <= actual && actual <= end_inclusive;
         message =
           Condition
             { actual_str = string_of_float actual;
               description = Printf.sprintf "be between %f and %f" start_inclusive end_inclusive } } )

let strictly_between start_exclusive end_exclusive =
  Assertion
    (fun actual ->
       { is_success = start_exclusive < actual && actual < end_exclusive;
         message =
           Condition
             { actual_str = string_of_float actual;
               description =
                 Printf.sprintf "be strictly between %f and %f" start_exclusive end_exclusive } } )
