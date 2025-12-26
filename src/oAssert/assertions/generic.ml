open Shared.Compare
open Shared.Equal
open Shared.Value

module TypeOf (V : Values.VALUE) : VALUE_ASSERT with type t = V.t = ValueAssertions (V)

module EqOf (V : Values.EQ_VALUE) : EQUAL_ASSERT with type t = V.t = EqualAssertions (V)

module CmpOf (V : Values.CMP_VALUE) : COMPARE_ASSERT with type t = V.t = CompareAssertions (V)
