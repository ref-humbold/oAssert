open Internals

open struct
  module S = Stdlib.String
  module ST = Type.String
end

let empty =
  Assertion
    (fun actual ->
       build_assertion
         (S.equal actual "")
         (Equality {expected_str = "empty string"; actual_str = ST.to_string actual; negated = false}) )

let of_length length =
  Assertion
    (fun actual ->
       let actual_length = S.length actual in
       build_assertion
         (actual_length = length)
         (Condition
            { actual_str = string_of_int actual_length;
              description = Printf.sprintf "have length %d" length;
              negated = false } ) )

let equal expected =
  Assertion
    (fun actual ->
       build_assertion
         (S.equal expected actual)
         (Equality
            {expected_str = ST.to_string expected; actual_str = ST.to_string actual; negated = false}
         ) )

let uppercase =
  Assertion
    (fun actual ->
       build_assertion
         (S.uppercase_ascii actual = actual)
         (Condition
            {actual_str = ST.to_string actual; description = "be in uppercase"; negated = false} ) )

let lowercase =
  Assertion
    (fun actual ->
       build_assertion
         (S.lowercase_ascii actual = actual)
         (Condition
            {actual_str = ST.to_string actual; description = "be in lowercase"; negated = false} ) )

let starting_with prefix =
  Assertion
    (fun actual ->
       build_assertion
         (S.starts_with ~prefix actual)
         (Condition
            { actual_str = ST.to_string actual;
              description = Printf.sprintf "begin with %s" @@ ST.to_string prefix;
              negated = false } ) )

let ending_with suffix =
  Assertion
    (fun actual ->
       build_assertion
         (S.ends_with ~suffix actual)
         (Condition
            { actual_str = ST.to_string actual;
              description = Printf.sprintf "end with %s" @@ ST.to_string suffix;
              negated = false } ) )
