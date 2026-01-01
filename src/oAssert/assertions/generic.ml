open Shared.Compare
open Shared.Value

module TypeOf (V : Values.VALUE) : VALUE_ASSERT with type t = V.t = ValueAssertions (V)

module CompareTypeOf (V : Values.COMPARE_VALUE) : COMPARE_ASSERT with type t = V.t =
  CompareAssertions (V)
