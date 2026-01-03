open Internals
open Shared.Compare

open struct
  module IV = Values.Int
end

include CompareAssertions (IV)

let zero =
  Assertion
    (fun actual ->
       build_assertion
         (actual = 0)
         (ValueEquality {expected_str = "zero"; actual_str = IV.to_string actual}) )

let positive =
  Assertion
    (fun actual ->
       build_assertion
         (actual > 0)
         (Condition {actual_str = IV.to_string actual; description = "be positive"}) )

let negative =
  Assertion
    (fun actual ->
       build_assertion
         (actual < 0)
         (Condition {actual_str = IV.to_string actual; description = "be negative"}) )
