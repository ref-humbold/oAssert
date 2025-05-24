open Internals

open struct
  module F = Stdlib.Float
  module FT = Type.Float
end

let nan =
  Assertion
    (fun actual ->
       build_assertion
         (F.is_nan actual)
         (Equality {expected_str = "NaN"; actual_str = FT.to_string actual; negated = false}) )

let zero =
  Assertion
    (fun actual ->
       build_assertion
         (FT.equal 0.0 actual)
         (Equality {expected_str = "zero"; actual_str = FT.to_string actual; negated = false}) )

let positive =
  Assertion
    (fun actual ->
       build_assertion
         (actual > 0.0)
         (Condition {actual_str = FT.to_string actual; description = "be positive"; negated = false}) )

let negative =
  Assertion
    (fun actual ->
       build_assertion
         (actual < 0.0)
         (Condition {actual_str = FT.to_string actual; description = "be negative"; negated = false}) )

let close_to expected ~diff =
  Assertion
    (fun actual ->
       build_assertion
         (expected -. diff <= actual && actual <= expected +. diff)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be close to %f +/- %f" expected diff;
              negated = false } ) )

let greater_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual > expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be greater than %f" expected;
              negated = false } ) )

let greater_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual >= expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be greater than or equal to %f" expected;
              negated = false } ) )

let less_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual < expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be less than %f" expected;
              negated = false } ) )

let less_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual <= expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be less than or equal to %f" expected;
              negated = false } ) )

let between start_inclusive end_inclusive =
  Assertion
    (fun actual ->
       build_assertion
         (start_inclusive <= actual && actual <= end_inclusive)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be between %f and %f" start_inclusive end_inclusive;
              negated = false } ) )

let strictly_between start_exclusive end_exclusive =
  Assertion
    (fun actual ->
       build_assertion
         (start_exclusive < actual && actual < end_exclusive)
         (Condition
            { actual_str = FT.to_string actual;
              description =
                Printf.sprintf "be strictly between %f and %f" start_exclusive end_exclusive;
              negated = false } ) )
