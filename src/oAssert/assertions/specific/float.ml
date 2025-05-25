open Internals
open Constants

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

let equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (expected = actual)
         (Equality
            {expected_str = FT.to_string expected; actual_str = FT.to_string actual; negated = false}
         ) )

let close_to expected ~diff =
  if diff <= 0.0
  then invalid_arg @@ Printf.sprintf "%s: Difference should be greater than 0" __FUNCTION__
  else
    Assertion
      (fun actual ->
         build_assertion
           (abs_float (expected -. actual) <= diff)
           (Condition
              { actual_str = FT.to_string actual;
                description = Printf.sprintf "be close to %.12g with a margin of %.12g" expected diff;
                negated = false } ) )

let greater_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual > expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be greater than %.12g" expected;
              negated = false } ) )

let greater_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual >= expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be greater than or equal to %.12g" expected;
              negated = false } ) )

let less_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual < expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be less than %.12g" expected;
              negated = false } ) )

let less_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual <= expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be less than or equal to %.12g" expected;
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
            { actual_str = FT.to_string actual;
              description =
                Printf.sprintf
                  "be between %.12g (%s) and %.12g (%s)"
                  minimum
                  min_mode
                  maximum
                  max_mode;
              negated = false } ) )
