open Internals
open Constants

open struct
  module F = Stdlib.Float
  module FT = Values.Float
end

let nan =
  Assertion
    (fun actual ->
       build_assertion
         (F.is_nan actual)
         (Equality
            {expected_str = FT.to_string nan; actual_str = FT.to_string actual; negated = false} ) )

let zero =
  Assertion
    (fun actual ->
       build_assertion
         (FT.equal 0.0 actual)
         (Equality
            {expected_str = FT.to_string 0.0; actual_str = FT.to_string actual; negated = false} ) )

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
                description =
                  Printf.sprintf
                    "be close to %s with difference %s"
                    (FT.to_string expected)
                    (FT.to_string diff);
                negated = false } ) )

let greater_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual > expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be greater than %s" (FT.to_string expected);
              negated = false } ) )

let greater_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual >= expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be greater than or equal to %s" (FT.to_string expected);
              negated = false } ) )

let less_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual < expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be less than %s" (FT.to_string expected);
              negated = false } ) )

let less_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual <= expected)
         (Condition
            { actual_str = FT.to_string actual;
              description = Printf.sprintf "be less than or equal to %s" (FT.to_string expected);
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
                  "be between %s (%s) and %s (%s)"
                  (FT.to_string minimum)
                  min_mode
                  (FT.to_string maximum)
                  max_mode;
              negated = false } ) )
