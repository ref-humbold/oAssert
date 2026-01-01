open Internals
open Common
open Value

module type COMPARE_ASSERT = sig
  include VALUE_ASSERT

  val greater_than : t -> t assertion

  val greater_than_or_equal_to : t -> t assertion

  val less_than : t -> t assertion

  val less_than_or_equal_to : t -> t assertion

  val between : t interval_end -> t interval_end -> t assertion
end

module CompareAssertions (V : Values.COMPARE_VALUE) : COMPARE_ASSERT with type t = V.t = struct
  include ValueAssertions (V)

  let greater_than expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected > 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be greater than %s" (V.to_string expected) } ) )

  let greater_than_or_equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected >= 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be greater than or equal to %s" (V.to_string expected)
              } ) )

  let less_than expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected < 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be less than %s" (V.to_string expected) } ) )

  let less_than_or_equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (V.compare actual expected <= 0)
           (Condition
              { actual_str = V.to_string actual;
                description = Printf.sprintf "be less than or equal to %s" (V.to_string expected) }
           ) )

  let between minimum maximum =
    let string_of_interval_end ending =
      match ending with
      | Inclusive x -> Printf.sprintf "%s (inclusive)" (V.to_string x)
      | Exclusive x -> Printf.sprintf "%s (exclusive)" (V.to_string x)
    in
    let comparison actual =
      let min_condition =
        match minimum with
        | Inclusive m -> m <= actual
        | Exclusive m -> m < actual
      and max_condition =
        match maximum with
        | Inclusive m -> actual <= m
        | Exclusive m -> actual < m
      in
      min_condition && max_condition
    in
    Assertion
      (fun actual ->
         build_assertion
           (comparison actual)
           (Condition
              { actual_str = V.to_string actual;
                description =
                  Printf.sprintf
                    "be between %s and %s"
                    (string_of_interval_end minimum)
                    (string_of_interval_end maximum) } ) )
end
