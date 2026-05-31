(* Test params: Char assertions. *)

let params_whitespace = [' '; '\t'; '\n'; '\r'; '\012'; '\009']

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

let params_not_letters =
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
    '~';
    '0';
    '1';
    '2';
    '3';
    '4';
    '5';
    '6';
    '7';
    '8';
    '9' ]
