open Internals

open struct
  module BV = Values.Bool
end

let true_ =
  Assertion
    (fun actual ->
       build_assertion
         actual
         (Equality {expected_str = BV.to_string true; actual_str = BV.to_string actual}) )

let false_ =
  Assertion
    (fun actual ->
       build_assertion
         (not actual)
         (Equality {expected_str = BV.to_string false; actual_str = BV.to_string actual}) )
