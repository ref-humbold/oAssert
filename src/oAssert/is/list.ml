open Internals.Types
open Internals.Assert
module L = Stdlib.List

module type LIST_ASSERT = sig
  type elem

  val empty : elem list assertion

  val of_length : int -> elem list assertion
end

module Of (T : TYPE) : LIST_ASSERT with type elem = T.t = struct
  type elem = T.t

  let empty =
    Assertion
      (fun actual ->
         { is_success =
             ( match actual with
               | [] -> true
               | _ -> false );
           message =
             Equality {expected_str = "empty list"; actual_str = TypeMsg.list T.to_string actual} } )

  let of_length length =
    Assertion
      (fun actual ->
         let actual_length = L.length actual in
         { is_success = actual_length = length;
           message =
             Condition
               { actual_str = string_of_int actual_length;
                 description = Printf.sprintf "have length %d" length } } )
end
