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

let between minimum maximum =
  let description ending =
    match ending with
    | Inclusive x -> Printf.sprintf "%s (inclusive)" (FT.to_string x)
    | Exclusive x -> Printf.sprintf "%s (exclusive)" (FT.to_string x)
  in
  let comparison act =
    let min_condition =
      match minimum with
      | Inclusive m -> m <= act
      | Exclusive m -> m < act
    and max_condition =
      match maximum with
      | Inclusive m -> act <= m
      | Exclusive m -> act < m
    in
    min_condition && max_condition
  in
  Assertion
    (fun actual ->
       build_assertion
         (comparison actual)
         (Condition
            { actual_str = FT.to_string actual;
              description =
                Printf.sprintf "be between %s and %s" (description minimum) (description maximum);
              negated = false } ) )
