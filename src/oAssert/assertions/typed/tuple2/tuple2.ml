open Internals
open Shared.Equal

module OfEq (VF : Values.EQ_VALUE) (VS : Values.EQ_VALUE) :
  Assert.TUPLE2_ASSERT with type fst_elem = VF.t and type snd_elem = VS.t = struct
  open struct
    module TupleVal = Values.Tuple2.OfEq (VF) (VS)
  end

  type fst_elem = VF.t

  type snd_elem = VS.t

  type tuple2 = fst_elem * snd_elem

  include EqualAssertions (TupleVal)

  let first first =
    Assertion
      (fun actual ->
         build_assertion
           (VF.equal first @@ fst actual)
           (Condition
              { actual_str = TupleVal.to_string actual;
                description = Printf.sprintf "have first element %s" @@ VF.to_string first } ) )

  let second second =
    Assertion
      (fun actual ->
         build_assertion
           (VS.equal second @@ snd actual)
           (Condition
              { actual_str = TupleVal.to_string actual;
                description = Printf.sprintf "have second element %s" @@ VS.to_string second } ) )
end

module Of (VF : Values.VALUE) (VS : Values.VALUE) :
  Assert.TUPLE2_ASSERT with type fst_elem = VF.t and type snd_elem = VS.t =
  OfEq (Values.AsEq (VF)) (Values.AsEq (VS))
