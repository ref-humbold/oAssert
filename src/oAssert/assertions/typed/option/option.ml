open Internals

open struct
  module Opt = Stdlib.Option
end

module OfEq (V : Values.EQ_VALUE) : Helpers.OPTION_ASSERT with type elem = V.t = struct
  open struct
    module OptVal = Values.Option.OfEq (V)
  end

  type elem = V.t

  let none =
    Assertion
      (fun actual ->
         build_assertion
           (Opt.is_none actual)
           (Equality {expected_str = "None"; actual_str = OptVal.to_string actual}) )

  let some value =
    Assertion
      (fun actual ->
         build_assertion
           ( match actual with
             | Some x -> V.equal x value
             | None -> false )
           (Equality
              {expected_str = OptVal.to_string (Some value); actual_str = OptVal.to_string actual} ) )
end

module Of (V : Values.VALUE) : Helpers.OPTION_ASSERT with type elem = V.t = OfEq (Values.AsEq (V))
