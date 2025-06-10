open Internals

open struct
  module L = Stdlib.List
end

module OfEq (V : Values.EQ_VALUE) : Helpers.LIST_ASSERT with type elem = V.t = struct
  type elem = V.t

  module ListVal = Values.List.OfEq (V)

  let empty =
    Assertion
      (fun actual ->
         build_assertion
           ( match actual with
             | [] -> true
             | _ -> false )
           (Equality
              {expected_str = "empty list"; actual_str = ListVal.to_string actual; negated = false}
           ) )

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
           (ListVal.equal expected actual)
           (Equality
              { expected_str = ListVal.to_string expected;
                actual_str = ListVal.to_string actual;
                negated = false } ) )

  let containing element =
    Assertion
      (fun actual ->
         build_assertion
           (L.exists (fun e -> V.equal e element) actual)
           (Condition
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "contain %s" @@ V.to_string element;
                negated = false } ) )
end

module Of (V : Values.VALUE) : Helpers.LIST_ASSERT with type elem = V.t = OfEq (Values.AsEq (V))
