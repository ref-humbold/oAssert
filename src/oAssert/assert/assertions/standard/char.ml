open Internals
open Shared.Compare

open struct
  module C = Stdlib.Char
  module CV = Values.Char
end

include CompareAssertions (CV)

let uppercase =
  Assertion
    (fun actual ->
       build_assertion
         ~no_negate:()
         (C.uppercase_ascii actual = actual)
         (Condition {actual_str = CV.to_string actual; description = "be an uppercase character"}) )

let lowercase =
  Assertion
    (fun actual ->
       build_assertion
         ~no_negate:()
         (C.lowercase_ascii actual = actual)
         (Condition {actual_str = CV.to_string actual; description = "be a lowercase character"}) )

let whitespace =
  let is_whitespace c =
    match c with
    | ' ' | '\t' | '\n' | '\r' | '\x0b' | '\x0c' -> true
    | _ -> false
  in
  Assertion
    (fun actual ->
       build_assertion
         (is_whitespace actual)
         (Condition {actual_str = CV.to_string actual; description = "be a whitespace character"}) )

let digit =
  let is_digit c =
    match c with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
    | _ -> false
  in
  Assertion
    (fun actual ->
       build_assertion
         (is_digit actual)
         (Condition {actual_str = CV.to_string actual; description = "be a digit"}) )
