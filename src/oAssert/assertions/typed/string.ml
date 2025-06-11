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
         (Equality {expected_str = "empty string"; actual_str = SV.to_string actual; negated = false}) )

let of_length length =
  Assertion
    (fun actual ->
       let actual_length = S.length actual in
       build_assertion
         (length = actual_length)
         (Comparison
            { actual_str = SV.to_string actual;
              description = Printf.sprintf "have length %d" length;
              result_str = Printf.sprintf "was %d" actual_length;
              negated = false } ) )

let equal expected =
  Assertion
    (fun actual ->
       build_assertion
         (SV.equal expected actual)
         (Equality
            {expected_str = SV.to_string expected; actual_str = SV.to_string actual; negated = false}
         ) )

let uppercase =
  Assertion
    (fun actual ->
       build_assertion
         (S.uppercase_ascii actual = actual)
         (Condition
            {actual_str = SV.to_string actual; description = "be in uppercase"; negated = false} ) )

let lowercase =
  Assertion
    (fun actual ->
       build_assertion
         (S.lowercase_ascii actual = actual)
         (Condition
            {actual_str = SV.to_string actual; description = "be in lowercase"; negated = false} ) )

let starting_with prefix =
  Assertion
    (fun actual ->
       build_assertion
         (S.starts_with ~prefix actual)
         (Condition
            { actual_str = SV.to_string actual;
              description = Printf.sprintf "begin with %s" @@ SV.to_string prefix;
              negated = false } ) )

let ending_with suffix =
  Assertion
    (fun actual ->
       build_assertion
         (S.ends_with ~suffix actual)
         (Condition
            { actual_str = SV.to_string actual;
              description = Printf.sprintf "end with %s" @@ SV.to_string suffix;
              negated = false } ) )
