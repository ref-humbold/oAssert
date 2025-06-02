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

let between minimum maximum =
  let description ending =
    match ending with
    | Inclusive x -> Printf.sprintf "%s (inclusive)" (IT.to_string x)
    | Exclusive x -> Printf.sprintf "%s (exclusive)" (IT.to_string x)
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
            { actual_str = IT.to_string actual;
              description =
                Printf.sprintf "be between %s and %s" (description minimum) (description maximum);
              negated = false } ) )
