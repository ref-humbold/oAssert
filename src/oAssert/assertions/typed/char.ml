open Internals
open Shared.Compare

open struct
  module C = Stdlib.Char
  module CV = Values.Char
  module CharSet = Set.Make (C)
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
  let whitespace_chars = CharSet.of_list [' '; '\t'; '\n'; '\r'; '\x0b'; '\x0c'] in
  Assertion
    (fun actual ->
       build_assertion
         (CharSet.mem actual whitespace_chars)
         (Condition {actual_str = CV.to_string actual; description = "be a whitespace character"}) )
