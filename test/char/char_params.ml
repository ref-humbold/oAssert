(* Test params: Char assertions. *)

let params_whitespace = [' '; '\t'; '\n'; '\r'; '\x0b'; '\x0c']

let params_digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let params_uppercase_letters =
  [ 'A';
    'B';
    'C';
    'D';
    'E';
    'F';
    'G';
    'H';
    'I';
    'J';
    'K';
    'L';
    'M';
    'N';
    'O';
    'P';
    'Q';
    'R';
    'S';
    'T';
    'U';
    'V';
    'W';
    'X';
    'Y';
    'Z' ]

let params_lowercase_letters =
  [ 'a';
    'b';
    'c';
    'd';
    'e';
    'f';
    'g';
    'h';
    'i';
    'j';
    'k';
    'l';
    'm';
    'n';
    'o';
    'p';
    'q';
    'r';
    's';
    't';
    'u';
    'v';
    'w';
    'x';
    'y';
    'z' ]

let params_symbols =
  [ '!';
    '@';
    '#';
    '$';
    '%';
    '^';
    '&';
    '*';
    '(';
    ')';
    '[';
    ']';
    '{';
    '}';
    '-';
    '_';
    '=';
    '+';
    '<';
    '>';
    '/';
    '\\';
    ',';
    '.';
    '\'';
    '"';
    ';';
    ':';
    '?';
    '~' ]

let params_all =
  params_uppercase_letters @ params_lowercase_letters @ params_digits @ params_symbols
  @ params_whitespace

(* fst < snd *)
let params_char_pairs =
  let rec pairing lst =
    match lst with
    | x :: y :: lst' -> (x, y) :: pairing lst'
    | _ -> []
  in
  pairing @@ List.sort compare params_all
