(* Test params: Int assertions. *)

let params_positive =
  [ 1;
    2;
    3;
    5;
    8;
    13;
    21;
    34;
    55;
    89;
    144;
    233;
    377;
    610;
    987;
    1597;
    2584;
    4181;
    6765;
    10946;
    17711;
    28657;
    46368;
    75025;
    121393;
    196418;
    317811;
    514229;
    832040;
    1000000 ]

let params_negative = List.map (fun x -> -x) params_positive

let params_number_pairs =
  let rec pairing lst =
    match lst with
    | x :: y :: lst' -> (x, y) :: pairing lst'
    | _ -> []
  in
  pairing params_positive
