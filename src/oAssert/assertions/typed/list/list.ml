open Internals

open struct
  module L = Stdlib.List
end

module OfEq (V : Values.EQ_VALUE) : Helpers.LIST_ASSERT with type elem = V.t = struct
  open struct
    module ListVal = Values.List.OfEq (V)

    module Locals = struct
      let contains element list = L.exists (fun e -> V.equal e element) list
    end
  end

  type elem = V.t

  let empty =
    Assertion
      (fun actual ->
         build_assertion
           (L.is_empty actual)
           (Equality
              {expected_str = "empty list"; actual_str = ListVal.to_string actual; negated = false}
           ) )

  let of_length length =
    Assertion
      (fun actual ->
         let actual_length = L.length actual in
         build_assertion
           (actual_length = length)
           (ConditionResult
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
           (Locals.contains element actual)
           (Condition
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "contain %s" @@ V.to_string element;
                negated = false } ) )

  let containing_all elements =
    Assertion
      (fun actual ->
         let missing =
           L.filter_map (fun e -> if Locals.contains e actual then None else Some e) elements
         in
         build_assertion
           (L.is_empty missing)
           (ConditionResult
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "contain all values of %s" @@ ListVal.to_string elements;
                result_str = Printf.sprintf "%s are missing" @@ ListVal.to_string missing;
                negated = false } ) )

  let containing_any elements =
    Assertion
      (fun actual ->
         build_assertion
           (L.exists (fun e -> Locals.contains e actual) elements)
           (Condition
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "contain any value of %s" @@ ListVal.to_string elements;
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

  let containing_all elements =
    Assertion
      (fun actual ->
         let missing = L.filter_map (fun e -> if L.mem e actual then None else Some e) elements in
         build_assertion
           (L.is_empty missing)
           (ConditionResult
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "contain all values of %s" @@ ListVal.to_string elements;
                result_str = Printf.sprintf "%s are missing" @@ ListVal.to_string missing;
                negated = false } ) )

  let containing_any elements =
    Assertion
      (fun actual ->
         build_assertion
           (L.exists (fun e -> L.mem e actual) elements)
           (Condition
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "contain any value of %s" @@ ListVal.to_string elements;
                negated = false } ) )
end
