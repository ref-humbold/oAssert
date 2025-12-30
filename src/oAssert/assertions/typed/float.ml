open Internals
open Common
open Shared.Compare

open struct
  module F = Stdlib.Float
  module FV = Values.Float
end

include CompareAssertions (FV)

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
         (actual = 0.0)
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

let close_to expected ~diff =
  Assertion
    (fun actual ->
       let diff_value =
         match diff with
         | Difference d ->
           if d >= 0.0
           then d
           else invalid_arg @@ Printf.sprintf "%s: Difference should be greater than 0" __FUNCTION__
       in
       let actual_diff = abs_float (expected -. actual) in
       build_assertion
         (actual_diff <= diff_value)
         (ConditionResult
            { actual_str = FV.to_string actual;
              description =
                Printf.sprintf
                  "be close to %s with difference %s"
                  (FV.to_string expected)
                  (FV.to_string diff_value);
              result_str = Printf.sprintf "difference was %s" (FV.to_string actual_diff) } ) )
