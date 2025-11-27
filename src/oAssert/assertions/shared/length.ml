open Internals

module type LENGTH_ASSERT = sig
  type collection

  val zero : collection assertion

  val one : collection assertion

  val equal_to : int -> collection assertion

  val greater_than : int -> collection assertion

  val greater_than_or_equal_to : int -> collection assertion

  val less_than : int -> collection assertion

  val less_than_or_equal_to : int -> collection assertion

  val same_as : collection -> collection assertion
end

module LengthAssertions (LEN : sig
    module V : Values.VALUE

    val get_length : V.t -> int
  end) : LENGTH_ASSERT with type collection = LEN.V.t = struct
  type collection = LEN.V.t

  let zero =
    Assertion
      (fun actual ->
         let actual_length = LEN.get_length actual in
         build_assertion
           (actual_length = 0)
           (ConditionResult
              { actual_str = LEN.V.to_string actual;
                description = "have length 0";
                result_str = Printf.sprintf "was %d" actual_length } ) )

  let one =
    Assertion
      (fun actual ->
         let actual_length = LEN.get_length actual in
         build_assertion
           (actual_length = 1)
           (ConditionResult
              { actual_str = LEN.V.to_string actual;
                description = "have length 1";
                result_str = Printf.sprintf "was %d" actual_length } ) )

  let equal_to length =
    Assertion
      (fun actual ->
         let actual_length = LEN.get_length actual in
         build_assertion
           (actual_length = length)
           (ConditionResult
              { actual_str = LEN.V.to_string actual;
                description = Printf.sprintf "have length %d" length;
                result_str = Printf.sprintf "was %d" actual_length } ) )

  let greater_than length =
    Assertion
      (fun actual ->
         let actual_length = LEN.get_length actual in
         build_assertion
           (actual_length > length)
           (ConditionResult
              { actual_str = LEN.V.to_string actual;
                description = Printf.sprintf "have length greater than %d" length;
                result_str = Printf.sprintf "was %d" actual_length } ) )

  let greater_than_or_equal_to length =
    Assertion
      (fun actual ->
         let actual_length = LEN.get_length actual in
         build_assertion
           (actual_length >= length)
           (ConditionResult
              { actual_str = LEN.V.to_string actual;
                description = Printf.sprintf "have length greater than or equal to %d" length;
                result_str = Printf.sprintf "was %d" actual_length } ) )

  let less_than length =
    Assertion
      (fun actual ->
         let actual_length = LEN.get_length actual in
         build_assertion
           (actual_length < length)
           (ConditionResult
              { actual_str = LEN.V.to_string actual;
                description = Printf.sprintf "have length less than %d" length;
                result_str = Printf.sprintf "was %d" actual_length } ) )

  let less_than_or_equal_to length =
    Assertion
      (fun actual ->
         let actual_length = LEN.get_length actual in
         build_assertion
           (actual_length <= length)
           (ConditionResult
              { actual_str = LEN.V.to_string actual;
                description = Printf.sprintf "have length less than or equal to %d" length;
                result_str = Printf.sprintf "was %d" actual_length } ) )

  let same_as collection =
    Assertion
      (fun actual ->
         let actual_length = LEN.get_length actual and expected_length = LEN.get_length collection in
         build_assertion
           (actual_length = expected_length)
           (ConditionResult
              { actual_str = LEN.V.to_string actual;
                description =
                  Printf.sprintf
                    "have same length as %s, which is %d"
                    (LEN.V.to_string collection)
                    expected_length;
                result_str = Printf.sprintf "was %d" actual_length } ) )
end
