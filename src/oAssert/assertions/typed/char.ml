open Internals

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
