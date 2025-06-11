open Internals

open struct
  module L = Stdlib.List
end

module OfEq (V : Values.EQ_VALUE) : Helpers.LIST_ASSERT with type elem = V.t = struct
  open struct
    module ListVal = Values.List.OfEq (V)
  end

  type elem = V.t

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
           (Comparison
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "have length %d" length;
                result_str = Printf.sprintf "was %d" actual_length;
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

module Of (V : Values.VALUE) : Helpers.LIST_ASSERT with type elem = V.t = struct
  open struct
    module ListVal = Values.List.Of (V)
  end

  include OfEq (Values.AsEq (V))

  let containing element =
    Assertion
      (fun actual ->
         build_assertion
           (L.mem element actual)
           (Condition
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "contain %s" @@ V.to_string element;
                negated = false } ) )
end
