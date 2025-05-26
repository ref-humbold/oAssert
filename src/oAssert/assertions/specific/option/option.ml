open Internals

open struct
  module Opt = Stdlib.Option
end

module OfEquatable (V : Values.EQUATABLE_VALUE) : Helpers.OPTION_ASSERT with type elem = V.t =
struct
  type elem = V.t

  let none =
    Assertion
      (fun actual ->
         build_assertion
           (Opt.is_none actual)
           (Equality
              { expected_str = "None";
                actual_str = Helpers.string_of V.to_string actual;
                negated = false } ) )

  let some value =
    Assertion
      (fun actual ->
         build_assertion
           ( match actual with
             | Some x -> V.equal x value
             | None -> false )
           (Equality
              { expected_str = Helpers.string_of V.to_string (Some value);
                actual_str = Helpers.string_of V.to_string actual;
                negated = false } ) )
end

module Of (V : Values.VALUE) : Helpers.OPTION_ASSERT with type elem = V.t =
  OfEquatable (Values.AsEquatable (V))
