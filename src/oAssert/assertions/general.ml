open Internals.Common
open Internals.Type_assert

let true_ =
  Assertion
    (fun actual ->
       { is_success = actual;
         message = Equality {expected_str = string_of_bool true; actual_str = string_of_bool actual}
       } )

let false_ =
  Assertion
    (fun actual ->
       { is_success = not actual;
         message = Equality {expected_str = string_of_bool true; actual_str = string_of_bool actual}
       } )

module Type (T : TYPE) : TYPE_ASSERT with type t = T.t = struct
  type t = T.t

  let equal_to expected =
    Assertion
      (fun actual ->
         { is_success = expected = actual;
           message = Equality {expected_str = T.to_string expected; actual_str = T.to_string actual}
         } )
end
