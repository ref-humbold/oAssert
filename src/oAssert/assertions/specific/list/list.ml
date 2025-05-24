open Internals

open struct
  module L = Stdlib.List
end

module Of (T : Type.TYPE) : Helpers.LIST_ASSERT with type elem = T.t = struct
  type elem = T.t

  let empty =
    Assertion
      (fun actual ->
         build_assertion
           ( match actual with
             | [] -> true
             | _ -> false )
           (Equality
              { expected_str = "empty list";
                actual_str = Helpers.string_of T.to_string actual;
                negated = false } ) )

  let of_length length =
    Assertion
      (fun actual ->
         let actual_length = L.length actual in
         build_assertion
           (actual_length = length)
           (Condition
              { actual_str = string_of_int actual_length;
                description = Printf.sprintf "have length %d" length;
                negated = false } ) )

  let equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (expected = actual)
           (Equality
              { expected_str = Helpers.string_of T.to_string expected;
                actual_str = Helpers.string_of T.to_string actual;
                negated = false } ) )

  let containing element =
    Assertion
      (fun actual ->
         build_assertion
           (L.mem element actual)
           (Condition
              { actual_str = Helpers.string_of T.to_string actual;
                description = Printf.sprintf "contain %s" @@ T.to_string element;
                negated = false } ) )
end
