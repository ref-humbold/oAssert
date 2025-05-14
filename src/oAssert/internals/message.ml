type assertion_message =
  | Equality of {expected_str : string; actual_str : string}
  | Condition of {actual_str : string; description : string}

let build_message msg =
  match msg with
  | Equality {expected_str; actual_str} ->
    Printf.sprintf "Expected %s, but was %s" expected_str actual_str
  | Condition {actual_str; description} -> Printf.sprintf "Expected %s to %s" actual_str description

module TypeMsg = struct
  let option printer opt =
    match opt with
    | Some x -> Printf.sprintf "Some %s" @@ printer x
    | None -> "None"

  let list printer lst = Printf.sprintf "[%s]" @@ String.concat "; " @@ List.map printer lst
end
