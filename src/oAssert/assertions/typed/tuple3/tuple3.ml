open Internals

module OfEq (VF : Values.EQ_VALUE) (VS : Values.EQ_VALUE) (VT : Values.EQ_VALUE) :
  Helpers.TUPLE3_ASSERT with type fst_elem = VF.t and type snd_elem = VS.t and type trd_elem = VT.t =
struct
  open struct
    module TupleVal = Values.Tuple3.OfEq (VF) (VS) (VT)
  end

  type fst_elem = VF.t

  type snd_elem = VS.t

  type trd_elem = VT.t

  type tuple3 = fst_elem * snd_elem * trd_elem

  let equal_to expected =
    Assertion
      (fun actual ->
         build_assertion
           (TupleVal.equal expected actual)
           (Equality
              {expected_str = TupleVal.to_string expected; actual_str = TupleVal.to_string actual} ) )

  let with_first first =
    Assertion
      (fun actual ->
         let act1, _, _ = actual in
         build_assertion
           (VF.equal first act1)
           (Condition
              { actual_str = TupleVal.to_string actual;
                description = Printf.sprintf "have first element %s" @@ VF.to_string first } ) )

  let with_second second =
    Assertion
      (fun actual ->
         let _, act2, _ = actual in
         build_assertion
           (VS.equal second act2)
           (Condition
              { actual_str = TupleVal.to_string actual;
                description = Printf.sprintf "have second element %s" @@ VS.to_string second } ) )

  let with_third third =
    Assertion
      (fun actual ->
         let _, _, act3 = actual in
         build_assertion
           (VT.equal third act3)
           (Condition
              { actual_str = TupleVal.to_string actual;
                description = Printf.sprintf "have third element %s" @@ VT.to_string third } ) )
end

module Of (VF : Values.VALUE) (VS : Values.VALUE) (VT : Values.VALUE) :
  Helpers.TUPLE3_ASSERT with type fst_elem = VF.t and type snd_elem = VS.t and type trd_elem = VT.t =
  OfEq (Values.AsEq (VF)) (Values.AsEq (VS)) (Values.AsEq (VT))
