open Internals
open Shared.Value

module Of (VF : Values.VALUE) (VS : Values.VALUE) :
  Sigs.TUPLE2_ASSERT with type fst_elem = VF.t and type snd_elem = VS.t = struct
  open struct
    module TupleVal = Values.Tuple2.Of (VF) (VS)
  end

  type fst_elem = VF.t

  type snd_elem = VS.t

  type tuple2 = fst_elem * snd_elem

  include ValueAssertions (TupleVal)

  let first first =
    Assertion
      (fun actual ->
         build_assertion
           (VF.equal first @@ fst actual)
           (Condition
              { actual_str = TupleVal.to_string actual;
                description = Printf.sprintf "have first element equal to %s" @@ VF.to_string first
              } ) )

  let second second =
    Assertion
      (fun actual ->
         build_assertion
           (VS.equal second @@ snd actual)
           (Condition
              { actual_str = TupleVal.to_string actual;
                description = Printf.sprintf "have second element equal to %s" @@ VS.to_string second
              } ) )
end
