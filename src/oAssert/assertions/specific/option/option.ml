open Internals
open Type_assert

open struct
  module Opt = Stdlib.Option
end

module Of (T : TYPE) : Helpers.OPTION_ASSERT with type elem = T.t = struct
  type elem = T.t

  let none =
    Assertion
      (fun actual ->
         build_assertion
           (Opt.is_none actual)
           (Equality
              { expected_str = "None";
                actual_str = Helpers.string_of T.to_string actual;
                negated = false } ) )

  let some value =
    Assertion
      (fun actual ->
         build_assertion
           ( match actual with
             | Some a -> a = value
             | None -> false )
           (Equality
              { expected_str = Helpers.string_of T.to_string (Some value);
                actual_str = Helpers.string_of T.to_string actual;
                negated = false } ) )
end
