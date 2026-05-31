(* Test params: List assertions. *)

let generate_params max_len start =
  let rec generate n i acc =
    if i >= n
    then acc
    else
      let new_list = if List.is_empty acc then List.init i (fun x -> x) else i :: List.hd acc in
      generate n (i + 1) (new_list :: acc)
  in
  generate max_len start []

let params_short_lists = generate_params 25 0

let params_long_lists = generate_params 100 75

let params_all_lists = params_short_lists @ params_long_lists

let params_non_empty_lists = List.filter (fun p -> not @@ List.is_empty p) params_all_lists
