open Internals

open struct
  module Opt = Stdlib.Option
end

module Of (V : Values.VALUE) : Assert.OPTION_ASSERT with type elem = V.t = struct
  open struct
    module OptVal = Values.Option.Of (V)
  end

  type elem = V.t

  let none =
    Assertion
      (fun actual ->
         build_assertion
           (Opt.is_none actual)
           (ValueEquality {expected_str = "None"; actual_str = OptVal.to_string actual}) )

  let some value =
    Assertion
      (fun actual ->
         build_assertion
           ( match actual with
             | Some x -> V.equal x value
             | None -> false )
           (Condition
              { actual_str = OptVal.to_string actual;
                description = Printf.sprintf "be equal to %s" @@ OptVal.to_string (Some value) } ) )

  let value_matching predicate =
    Assertion
      (fun actual ->
         let is_match =
           match actual with
           | Some x -> predicate x
           | None -> false
         in
         build_assertion
           is_match
           (Condition
              { actual_str = OptVal.to_string actual;
                description = "have value matching given predicate" } ) )
end
