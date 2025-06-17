include Types

module Bool : CMP_VALUE with type t = bool = Stdlib.Bool

module Int : CMP_VALUE with type t = int = Stdlib.Int

module Float : CMP_VALUE with type t = float = Stdlib.Float

module Char : CMP_VALUE with type t = char = struct
  include Stdlib.Char

  let to_string = Stdlib.Char.escaped
end

module String : CMP_VALUE with type t = string = struct
  include Stdlib.String

  let to_string s = s
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
