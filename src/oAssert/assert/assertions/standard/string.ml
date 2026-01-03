open Internals
open Shared.Length
open Shared.Value

open struct
  module S = Stdlib.String
  module SV = Values.String
end

module Length = LengthAssertions (struct
    module V = SV

    let get_length = S.length
  end)

include ValueAssertions (SV)

let empty =
  Assertion
    (fun actual ->
       build_assertion
         (S.empty = actual)
         (EmptyValue {type_str = "string"; actual_str = SV.to_string actual}) )

let uppercase =
  Assertion
    (fun actual ->
       build_assertion
         ~no_negate:()
         (S.uppercase_ascii actual = actual)
         (Condition {actual_str = SV.to_string actual; description = "be all uppercase"}) )

let lowercase =
  Assertion
    (fun actual ->
       build_assertion
         ~no_negate:()
         (S.lowercase_ascii actual = actual)
         (Condition {actual_str = SV.to_string actual; description = "be all lowercase"}) )

let starting_with prefix =
  Assertion
    (fun actual ->
       build_assertion
         (S.starts_with ~prefix actual)
         (Condition
            { actual_str = SV.to_string actual;
              description = Printf.sprintf "begin with %s" @@ SV.to_string prefix } ) )

let ending_with suffix =
  Assertion
    (fun actual ->
       build_assertion
         (S.ends_with ~suffix actual)
         (Condition
            { actual_str = SV.to_string actual;
              description = Printf.sprintf "end with %s" @@ SV.to_string suffix } ) )

let containing_char character =
  Assertion
    (fun actual ->
       build_assertion
         (S.contains actual character)
         (Condition
            { actual_str = SV.to_string actual;
              description = Printf.sprintf "contain character %C" character } ) )
