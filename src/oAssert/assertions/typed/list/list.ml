open Internals.Common
open Internals.Type_assert
module L = Stdlib.List

module Of (T : TYPE) : Helpers.LIST_ASSERT with type elem = T.t = struct
  type elem = T.t

  let empty =
    Assertion
      (fun actual ->
         { is_success =
             ( match actual with
               | [] -> true
               | _ -> false );
           message =
             Equality {expected_str = "empty list"; actual_str = Helpers.string_of T.to_string actual}
         } )

  let of_length length =
    Assertion
      (fun actual ->
         let actual_length = L.length actual in
         { is_success = actual_length = length;
           message =
             Condition
               { actual_str = string_of_int actual_length;
                 description = Printf.sprintf "have length %d" length } } )

  let equal_to expected =
    Assertion
      (fun actual ->
         { is_success = expected = actual;
           message =
             Equality
               { expected_str = Helpers.string_of T.to_string expected;
                 actual_str = Helpers.string_of T.to_string actual } } )

  let containing element =
    Assertion
      (fun actual ->
         { is_success = L.mem element actual;
           message =
             Condition
               { actual_str = Helpers.string_of T.to_string actual;
                 description = Printf.sprintf "contain %s" @@ T.to_string element } } )
end
