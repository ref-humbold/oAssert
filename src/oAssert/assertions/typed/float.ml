open Internals
open Common

open struct
  module F = Stdlib.Float
  module FV = Values.Float
end

let nan =
  Assertion
    (fun actual ->
       build_assertion
         (F.is_nan actual)
         (Equality {expected_str = FV.to_string nan; actual_str = FV.to_string actual}) )

let zero =
  Assertion
    (fun actual ->
       build_assertion
         (FV.equal 0.0 actual)
         (Equality {expected_str = FV.to_string 0.0; actual_str = FV.to_string actual}) )

let positive =
  Assertion
    (fun actual ->
       build_assertion
         (actual > 0.0)
         (Condition {actual_str = FV.to_string actual; description = "be positive"}) )

let negative =
  Assertion
    (fun actual ->
       build_assertion
         (actual < 0.0)
         (Condition {actual_str = FV.to_string actual; description = "be negative"}) )

let equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (expected = actual)
         (Equality {expected_str = FV.to_string expected; actual_str = FV.to_string actual}) )

let close_to expected ~diff =
  if diff <= 0.0
  then invalid_arg @@ Printf.sprintf "%s: Difference should be greater than 0" __FUNCTION__
  else
    Assertion
      (fun actual ->
         let actual_diff = abs_float (expected -. actual) in
         build_assertion
           (actual_diff <= diff)
           (ConditionResult
              { actual_str = FV.to_string actual;
                description =
                  Printf.sprintf
                    "be close to %s with difference %s"
                    (FV.to_string expected)
                    (FV.to_string diff);
                result_str = Printf.sprintf "difference was %s" (FV.to_string actual_diff) } ) )

let greater_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual > expected)
         (Condition
            { actual_str = FV.to_string actual;
              description = Printf.sprintf "be greater than %s" (FV.to_string expected) } ) )

let greater_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual >= expected)
         (Condition
            { actual_str = FV.to_string actual;
              description = Printf.sprintf "be greater than or equal to %s" (FV.to_string expected)
            } ) )

let less_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual < expected)
         (Condition
            { actual_str = FV.to_string actual;
              description = Printf.sprintf "be less than %s" (FV.to_string expected) } ) )

let less_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual <= expected)
         (Condition
            { actual_str = FV.to_string actual;
              description = Printf.sprintf "be less than or equal to %s" (FV.to_string expected) } ) )

let between minimum maximum =
  let description ending =
    match ending with
    | Inclusive x -> Printf.sprintf "%s (inclusive)" (FV.to_string x)
    | Exclusive x -> Printf.sprintf "%s (exclusive)" (FV.to_string x)
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
            { actual_str = FV.to_string actual;
              description =
                Printf.sprintf "be between %s and %s" (description minimum) (description maximum) }
         ) )
