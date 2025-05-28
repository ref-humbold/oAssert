open Internals
open Constants

open struct
  module IT = Values.Int
end

let zero =
  Assertion
    (fun actual ->
       build_assertion
         (IT.equal 0 actual)
         (Equality {expected_str = IT.to_string 0; actual_str = IT.to_string actual; negated = false}) )

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
         (IT.equal expected actual)
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
              description = Printf.sprintf "be greater than %s" (IT.to_string expected);
              negated = false } ) )

let greater_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual >= expected)
         (Condition
            { actual_str = IT.to_string actual;
              description = Printf.sprintf "be greater than or equal to %s" (IT.to_string expected);
              negated = false } ) )

let less_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual < expected)
         (Condition
            { actual_str = IT.to_string actual;
              description = Printf.sprintf "be less than %s" (IT.to_string expected);
              negated = false } ) )

let less_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual <= expected)
         (Condition
            { actual_str = IT.to_string actual;
              description = Printf.sprintf "be less than or equal to %s" (IT.to_string expected);
              negated = false } ) )

let between ?(mode = ClosedClosed) minimum maximum =
  let min_mode, max_mode =
    match mode with
    | ClosedClosed -> ("inclusive", "inclusive")
    | OpenOpen -> ("exclusive", "exclusive")
    | OpenClosed -> ("exclusive", "inclusive")
    | ClosedOpen -> ("inclusive", "exclusive")
  in
  let comparison actual =
    match mode with
    | ClosedClosed -> minimum <= actual && actual <= maximum
    | OpenOpen -> minimum < actual && actual < maximum
    | OpenClosed -> minimum < actual && actual <= maximum
    | ClosedOpen -> minimum <= actual && actual < maximum
  in
  Assertion
    (fun actual ->
       build_assertion
         (comparison actual)
         (Condition
            { actual_str = IT.to_string actual;
              description =
                Printf.sprintf
                  "be between %s (%s) and %s (%s)"
                  (IT.to_string minimum)
                  min_mode
                  (IT.to_string maximum)
                  max_mode;
              negated = false } ) )
