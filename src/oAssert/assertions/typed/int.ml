open Internals
open Common
open Shared.Compare

open struct
  module IV = Values.Int
end

include CompareAssertions (IV)

let zero =
  Assertion
    (fun actual ->
       build_assertion
         (IV.equal 0 actual)
         (Equality {expected_str = IV.to_string 0; actual_str = IV.to_string actual}) )

let positive =
  Assertion
    (fun actual ->
       build_assertion
         (actual > 0)
         (Condition {actual_str = IV.to_string actual; description = "be positive"}) )

let negative =
  Assertion
    (fun actual ->
       build_assertion
         (actual < 0)
         (Condition {actual_str = IV.to_string actual; description = "be negative"}) )

let between minimum maximum =
  let description ending =
    match ending with
    | Inclusive x -> Printf.sprintf "%s (inclusive)" (IV.to_string x)
    | Exclusive x -> Printf.sprintf "%s (exclusive)" (IV.to_string x)
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
            { actual_str = IV.to_string actual;
              description =
                Printf.sprintf "be between %s and %s" (description minimum) (description maximum) }
         ) )
