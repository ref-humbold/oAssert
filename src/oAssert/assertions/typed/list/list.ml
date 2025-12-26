open Internals
open Shared.Length
open Shared.Equal

open struct
  module L = Stdlib.List
end

module OfEq (V : Values.EQ_VALUE) : Assert.LIST_ASSERT with type elem = V.t = struct
  open struct
    module ListVal = Values.List.OfEq (V)

    module Locals = struct
      let contains element list = L.exists (fun e -> V.equal e element) list
    end
  end

  type elem = V.t

  module Length = LengthAssertions (struct
      module V = ListVal

      let get_length = L.length
    end)

  include EqualAssertions (ListVal)

  let empty =
    Assertion
      (fun actual ->
         build_assertion
           (L.is_empty actual)
           (Equality {expected_str = "empty list"; actual_str = ListVal.to_string actual}) )

  let containing element =
    Assertion
      (fun actual ->
         build_assertion
           (Locals.contains element actual)
           (Condition
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "contain %s" @@ V.to_string element } ) )

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
                result_str = Printf.sprintf "%s are missing" @@ ListVal.to_string missing } ) )

  let containing_any elements =
    Assertion
      (fun actual ->
         build_assertion
           (L.exists (fun e -> Locals.contains e actual) elements)
           (ConditionResult
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "contain any value of %s" @@ ListVal.to_string elements;
                result_str = "none was found" } ) )

  let all_matching predicate =
    Assertion
      (fun actual ->
         let not_matched = L.filter_map (fun e -> if predicate e then None else Some e) actual in
         build_assertion
           (L.is_empty not_matched)
           (ConditionResult
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "have all elements matching given predicate";
                result_str = Printf.sprintf "%s did not match" @@ ListVal.to_string not_matched } ) )

  let any_matching predicate =
    Assertion
      (fun actual ->
         build_assertion
           (L.exists (fun e -> predicate e) actual)
           (ConditionResult
              { actual_str = ListVal.to_string actual;
                description = Printf.sprintf "have any element matching given predicate";
                result_str = "none matched" } ) )
end

module Of (V : Values.VALUE) : Assert.LIST_ASSERT with type elem = V.t = OfEq (Values.AsEq (V))
