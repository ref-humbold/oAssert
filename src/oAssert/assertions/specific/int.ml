open Internals

open struct
  module IT = Type.Int
end

let zero =
  Assertion
    (fun actual ->
       build_assertion
         (actual = 0)
         (Equality {expected_str = "zero"; actual_str = IT.to_string actual; negated = false}) )

let positive =
  Assertion
    (fun actual ->
       build_assertion
         (actual > 0)
         (Condition {actual_str = IT.to_string actual; description = "be positive"; negated = false}) )

let negative =
  Assertion
    (fun actual ->
       build_assertion
         (actual < 0)
         (Condition {actual_str = IT.to_string actual; description = "be negative"; negated = false}) )

let equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (expected = actual)
         (Equality
            {expected_str = IT.to_string expected; actual_str = IT.to_string actual; negated = false}
         ) )

let greater_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual > expected)
         (Condition
            { actual_str = IT.to_string actual;
              description = Printf.sprintf "be greater than %d" expected;
              negated = false } ) )

let greater_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual >= expected)
         (Condition
            { actual_str = IT.to_string actual;
              description = Printf.sprintf "be greater than or equal to %d" expected;
              negated = false } ) )

let less_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual < expected)
         (Condition
            { actual_str = IT.to_string actual;
              description = Printf.sprintf "be less than %d" expected;
              negated = false } ) )

let less_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual <= expected)
         (Condition
            { actual_str = IT.to_string actual;
              description = Printf.sprintf "be less than or equal to %d" expected;
              negated = false } ) )

let between start_inclusive end_inclusive =
  Assertion
    (fun actual ->
       build_assertion
         (start_inclusive <= actual && actual <= end_inclusive)
         (Condition
            { actual_str = IT.to_string actual;
              description = Printf.sprintf "be between %d and %d" start_inclusive end_inclusive;
              negated = false } ) )

let strictly_between start_exclusive end_exclusive =
  Assertion
    (fun actual ->
       build_assertion
         (start_exclusive < actual && actual < end_exclusive)
         (Condition
            { actual_str = IT.to_string actual;
              description =
                Printf.sprintf "be strictly between %d and %d" start_exclusive end_exclusive;
              negated = false } ) )
