open Internals

open struct
  module S = Stdlib.String
  module SV = Values.String
end

let empty =
  Assertion
    (fun actual ->
       build_assertion
         (SV.equal actual "")
         (Equality {expected_str = "empty string"; actual_str = SV.to_string actual}) )

let of_length length =
  Assertion
    (fun actual ->
       let actual_length = S.length actual in
       build_assertion
         (length = actual_length)
         (ConditionResult
            { actual_str = SV.to_string actual;
              description = Printf.sprintf "have length %d" length;
              result_str = Printf.sprintf "was %d" actual_length } ) )

let equal expected =
  Assertion
    (fun actual ->
       build_assertion
         (SV.equal expected actual)
         (Equality {expected_str = SV.to_string expected; actual_str = SV.to_string actual}) )

let uppercase =
  Assertion
    (fun actual ->
       build_assertion
         (S.uppercase_ascii actual = actual)
         (Condition {actual_str = SV.to_string actual; description = "be in uppercase"}) )

let lowercase =
  Assertion
    (fun actual ->
       build_assertion
         (S.lowercase_ascii actual = actual)
         (Condition {actual_str = SV.to_string actual; description = "be in lowercase"}) )

let starting_with prefix =
  Assertion
    (fun actual ->
       build_assertion
         (S.starts_with ~prefix actual)
         (Condition
            { actual_str = SV.to_string actual;
              description = Printf.sprintf "begin with %s" @@ SV.to_string prefix } ) )

let ending_with suffix =
  Assertion
    (fun actual ->
       build_assertion
         (S.ends_with ~suffix actual)
         (Condition
            { actual_str = SV.to_string actual;
              description = Printf.sprintf "end with %s" @@ SV.to_string suffix } ) )

let containing_char character =
  Assertion
    (fun actual ->
       build_assertion
         (S.contains actual character)
         (Condition
            { actual_str = SV.to_string actual;
              description = Printf.sprintf "contain character %c" character } ) )
