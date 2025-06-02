open Internals

module OfEquatable (VF : Values.EQUATABLE_VALUE) (VS : Values.EQUATABLE_VALUE) :
  Helpers.TUPLE2_ASSERT with type fst_elem = VF.t and type snd_elem = VS.t = struct
  type fst_elem = VF.t

  type snd_elem = VS.t

  type tuple2 = fst_elem * snd_elem

  let equal_to expected =
    Assertion
      (fun actual ->
         let elem1, elem2 = expected and act1, act2 = actual in
         build_assertion
           (VF.equal elem1 act1 && VS.equal elem2 act2)
           (Equality
              { expected_str = Helpers.string_of VF.to_string VS.to_string expected;
                actual_str = Helpers.string_of VF.to_string VS.to_string actual;
                negated = false } ) )

  let with_first first =
    Assertion
      (fun actual ->
         build_assertion
           (VF.equal first @@ fst actual)
           (Condition
              { actual_str = Helpers.string_of VF.to_string VS.to_string actual;
                description = Printf.sprintf "have first element %s" @@ VF.to_string first;
                negated = false } ) )

  let with_second second =
    Assertion
      (fun actual ->
         build_assertion
           (VS.equal second @@ snd actual)
           (Condition
              { actual_str = Helpers.string_of VF.to_string VS.to_string actual;
                description = Printf.sprintf "have second element %s" @@ VS.to_string second;
                negated = false } ) )
end

module Of (VF : Values.VALUE) (VS : Values.VALUE) :
  Helpers.TUPLE2_ASSERT with type fst_elem = VF.t and type snd_elem = VS.t =
  OfEquatable (Values.AsEquatable (VF)) (Values.AsEquatable (VS))
