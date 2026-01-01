type assertion_message =
  | EmptyValue of {type_str : string; actual_str : string}
  | ValueEquality of {expected_str : string; actual_str : string}
  | Condition of {actual_str : string; description : string}
  | ConditionResult of {actual_str : string; description : string; result_str : string}
  | Raising of {expected : exn; actual : exn option}
  | RaisingNothing of {actual : exn option}

type assertion_result =
  | Passed
  | Failed of bool * assertion_message
  | NegatablePassed of assertion_message
  | NegatableFailed of assertion_message

type 'a assertion = Assertion of ('a -> assertion_result)
