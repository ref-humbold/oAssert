open Internals

module type VALUE_ASSERT = sig
  type t

  val matching : (t -> bool) -> t assertion
end

module ValueAssertions (V : Values.VALUE) : VALUE_ASSERT with type t = V.t = struct
  type t = V.t

  let matching predicate =
    Assertion
      (fun actual ->
         build_assertion
           (predicate actual)
           (Condition
              {actual_str = V.to_string actual; description = "have value matching given predicate"}
           ) )
end
