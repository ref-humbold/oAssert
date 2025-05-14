type assertion_message =
  | Equality of {expected_str : string; actual_str : string}
  | Condition of {actual_str : string; description : string}

let build_message msg =
  match msg with
  | Equality {expected_str; actual_str} ->
    Printf.sprintf "Expected %s, but was %s" expected_str actual_str
  | Condition {actual_str; description} -> Printf.sprintf "Expected %s to %s" actual_str description
