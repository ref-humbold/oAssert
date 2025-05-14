open Internals.Types
open Internals.Assert
module Opt = Stdlib.Option

module type OPTION_ASSERT = sig
  type elem

  val none : elem option assertion

  val some : elem -> elem option assertion
end

module Of (T : TYPE) : OPTION_ASSERT with type elem = T.t = struct
  type elem = T.t

  let none =
    Assertion
      (fun actual ->
         { is_success = Opt.is_none actual;
           message = Equality {expected_str = "None"; actual_str = TypeMsg.option T.to_string actual}
         } )

  let some value =
    Assertion
      (fun actual ->
         { is_success =
             ( match actual with
               | Some a -> a = value
               | None -> false );
           message =
             Equality
               { expected_str = TypeMsg.option T.to_string (Some value);
                 actual_str = TypeMsg.option T.to_string actual } } )
end
