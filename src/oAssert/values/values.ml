include Types

module Bool : VALUE with type t = bool = Stdlib.Bool

module Int : COMPARE_VALUE with type t = int = Stdlib.Int

module Float : COMPARE_VALUE with type t = float = Stdlib.Float

module Char : COMPARE_VALUE with type t = char = struct
  include Stdlib.Char

  let to_string = Printf.sprintf "%C"
end

module String : VALUE with type t = string = struct
  include Stdlib.String

  let to_string = Printf.sprintf "%S"
end

module Option = struct
  include Option
end

module List = struct
  include List
end

module Tuple2 = struct
  include Tuple2
end

module Tuple3 = struct
  include Tuple3
end
