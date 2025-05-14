open Internals.Assert
module S = Stdlib.String

let empty =
  Assertion
    (fun actual ->
       { is_success = S.equal actual "";
         message =
           Equality
             {expected_str = "empty string"; actual_str = Printf.sprintf "\"%s\"" (S.escaped actual)}
       } )

let of_length length =
  Assertion
    (fun actual ->
       let actual_length = S.length actual in
       { is_success = actual_length = length;
         message =
           Condition
             { actual_str = string_of_int actual_length;
               description = Printf.sprintf "have length %d" length } } )

let equal expected =
  Assertion
    (fun actual ->
       { is_success = S.equal expected actual;
         message =
           Equality
             { expected_str = Printf.sprintf "\"%s\"" (S.escaped expected);
               actual_str = Printf.sprintf "\"%s\"" (S.escaped actual) } } )

let uppercase =
  Assertion
    (fun actual ->
       { is_success = S.uppercase_ascii actual = actual;
         message =
           Condition
             { actual_str = Printf.sprintf "\"%s\"" (S.escaped actual);
               description = "be in uppercase" } } )

let lowercase =
  Assertion
    (fun actual ->
       { is_success = S.lowercase_ascii actual = actual;
         message =
           Condition
             { actual_str = Printf.sprintf "\"%s\"" (S.escaped actual);
               description = "be in lowercase" } } )

let starting_with prefix =
  Assertion
    (fun actual ->
       { is_success = S.starts_with ~prefix actual;
         message =
           Condition
             { actual_str = Printf.sprintf "\"%s\"" (S.escaped actual);
               description = Printf.sprintf "begin with \"%s\"" (S.escaped prefix) } } )

let ending_with suffix =
  Assertion
    (fun actual ->
       { is_success = S.ends_with ~suffix actual;
         message =
           Condition
             { actual_str = Printf.sprintf "\"%s\"" (S.escaped actual);
               description = Printf.sprintf "end with \"%s\"" (S.escaped suffix) } } )
