(* Test params: Float assertions. *)

let params_positive =
  [ 1.1;
    2.2;
    3.3;
    5.4;
    8.5;
    13.6;
    21.7;
    34.8;
    55.9;
    89.0;
    144.11;
    233.12;
    377.13;
    610.14;
    987.15;
    1597.16;
    2584.17;
    4181.18;
    6765.19;
    10946.0;
    17711.21;
    28657.22;
    46368.23;
    75025.24;
    121393.25;
    196418.26;
    317811.27;
    514229.28;
    832040.29;
    1000000.0 ]

let params_negative = List.map (fun x -> -.x) params_positive

let params_number_pairs =
  let rec pairing lst =
    match lst with
    | x :: y :: lst' -> (x, y) :: pairing lst'
    | _ -> []
  in
  pairing params_positive
