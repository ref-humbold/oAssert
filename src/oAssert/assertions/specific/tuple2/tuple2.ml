open Internals

module OfEq (VF : Values.EQ_VALUE) (VS : Values.EQ_VALUE) :
  Helpers.TUPLE2_ASSERT with type fst_elem = VF.t and type snd_elem = VS.t = struct
  type fst_elem = VF.t

  type snd_elem = VS.t

  type tuple2 = fst_elem * snd_elem

  module TupleVal = Values.Tuple2.OfEq (VF) (VS)

  let equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (TupleVal.equal expected actual)
           (Equality
              { expected_str = TupleVal.to_string expected;
                actual_str = TupleVal.to_string actual;
                negated = false } ) )

  let with_first first =
    Assertion
      (fun actual ->
         build_assertion
           (VF.equal first @@ fst actual)
           (Condition
              { actual_str = TupleVal.to_string actual;
                description = Printf.sprintf "have first element %s" @@ VF.to_string first;
                negated = false } ) )

  let with_second second =
    Assertion
      (fun actual ->
         build_assertion
           (VS.equal second @@ snd actual)
           (Condition
              { actual_str = TupleVal.to_string actual;
                description = Printf.sprintf "have second element %s" @@ VS.to_string second;
                negated = false } ) )
end

module Of (VF : Values.VALUE) (VS : Values.VALUE) :
  Helpers.TUPLE2_ASSERT with type fst_elem = VF.t and type snd_elem = VS.t =
  OfEq (Values.AsEq (VF)) (Values.AsEq (VS))
