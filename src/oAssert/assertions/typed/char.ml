open Internals
open Constants

open struct
  module C = Stdlib.Char
  module CV = Values.Char
  module CharSet = Set.Make (C)
end

let equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (CV.equal expected actual)
         (Equality {expected_str = CV.to_string expected; actual_str = CV.to_string actual}) )

let greater_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual > expected)
         (Condition
            { actual_str = CV.to_string actual;
              description = Printf.sprintf "be greater than %s" (CV.to_string expected) } ) )

let greater_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual >= expected)
         (Condition
            { actual_str = CV.to_string actual;
              description = Printf.sprintf "be greater than or equal to %s" (CV.to_string expected)
            } ) )

let less_than expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual < expected)
         (Condition
            { actual_str = CV.to_string actual;
              description = Printf.sprintf "be less than %s" (CV.to_string expected) } ) )

let less_than_or_equal_to expected =
  Assertion
    (fun actual ->
       build_assertion
         (actual <= expected)
         (Condition
            { actual_str = CV.to_string actual;
              description = Printf.sprintf "be less than or equal to %s" (CV.to_string expected) } ) )

let between minimum maximum =
  let description ending =
    match ending with
    | Inclusive x -> Printf.sprintf "%s (inclusive)" (CV.to_string x)
    | Exclusive x -> Printf.sprintf "%s (exclusive)" (CV.to_string x)
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
            { actual_str = CV.to_string actual;
              description =
                Printf.sprintf "be between %s and %s" (description minimum) (description maximum) }
         ) )

let uppercase =
  Assertion
    (fun actual ->
       if C.uppercase_ascii actual = C.lowercase_ascii actual
       then PassAlways
       else
         build_assertion
           (C.uppercase_ascii actual = actual)
           (Condition {actual_str = CV.to_string actual; description = "be an uppercase character"}) )

let lowercase =
  Assertion
    (fun actual ->
       if C.uppercase_ascii actual = C.lowercase_ascii actual
       then PassAlways
       else
         build_assertion
           (C.lowercase_ascii actual = actual)
           (Condition {actual_str = CV.to_string actual; description = "be a lowercase character"}) )

let whitespace =
  let whitespace_chars = CharSet.of_list [' '; '\t'; '\n'; '\r'; '\x0b'; '\x0c'] in
  Assertion
    (fun actual ->
       build_assertion
         (CharSet.mem actual whitespace_chars)
         (Condition {actual_str = CV.to_string actual; description = "be a whitespace character"}) )
