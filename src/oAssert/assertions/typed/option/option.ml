open Internals.Common
open Internals.Type_assert
module Opt = Stdlib.Option

module Of (T : TYPE) : Helpers.OPTION_ASSERT with type elem = T.t = struct
  type elem = T.t

  let none =
    Assertion
      (fun actual ->
         { is_success = Opt.is_none actual;
           message =
             Equality {expected_str = "None"; actual_str = Helpers.string_of T.to_string actual} } )

  let some value =
    Assertion
      (fun actual ->
         { is_success =
             ( match actual with
               | Some a -> a = value
               | None -> false );
           message =
             Equality
               { expected_str = Helpers.string_of T.to_string (Some value);
                 actual_str = Helpers.string_of T.to_string actual } } )
end
