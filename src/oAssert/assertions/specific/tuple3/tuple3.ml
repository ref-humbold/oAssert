open Internals

module OfEquatable
    (VF : Values.EQUATABLE_VALUE)
    (VS : Values.EQUATABLE_VALUE)
    (VT : Values.EQUATABLE_VALUE) :
  Helpers.TUPLE3_ASSERT with type fst_elem = VF.t and type snd_elem = VS.t and type trd_elem = VT.t =
struct
  type fst_elem = VF.t

  type snd_elem = VS.t

  type trd_elem = VT.t

  type tuple3 = fst_elem * snd_elem * trd_elem

  let equal_to expected =
    Assertion
      (fun actual ->
         let elem1, elem2, elem3 = expected and act1, act2, act3 = actual in
         build_assertion
           (VF.equal elem1 act1 && VS.equal elem2 act2 && VT.equal elem3 act3)
           (Equality
              { expected_str = Helpers.string_of VF.to_string VS.to_string VT.to_string expected;
                actual_str = Helpers.string_of VF.to_string VS.to_string VT.to_string actual;
                negated = false } ) )

  let with_first first =
    Assertion
      (fun actual ->
         let act1, _, _ = actual in
         build_assertion
           (VF.equal first act1)
           (Condition
              { actual_str = Helpers.string_of VF.to_string VS.to_string VT.to_string actual;
                description = Printf.sprintf "have first element %s" @@ VF.to_string first;
                negated = false } ) )

  let with_second second =
    Assertion
      (fun actual ->
         let _, act2, _ = actual in
         build_assertion
           (VS.equal second act2)
           (Condition
              { actual_str = Helpers.string_of VF.to_string VS.to_string VT.to_string actual;
                description = Printf.sprintf "have second element %s" @@ VS.to_string second;
                negated = false } ) )

  let with_third third =
    Assertion
      (fun actual ->
         let _, _, act3 = actual in
         build_assertion
           (VT.equal third act3)
           (Condition
              { actual_str = Helpers.string_of VF.to_string VS.to_string VT.to_string actual;
                description = Printf.sprintf "have third element %s" @@ VT.to_string third;
                negated = false } ) )
end

module Of (VF : Values.VALUE) (VS : Values.VALUE) (VT : Values.VALUE) :
  Helpers.TUPLE3_ASSERT with type fst_elem = VF.t and type snd_elem = VS.t and type trd_elem = VT.t =
  OfEquatable (Values.AsEquatable (VF)) (Values.AsEquatable (VS)) (Values.AsEquatable (VT))
