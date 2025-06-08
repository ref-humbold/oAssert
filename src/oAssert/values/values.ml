include Types

module Bool : CMP_VALUE with type t = bool = Stdlib.Bool

module Int : CMP_VALUE with type t = int = Stdlib.Int

module Float : CMP_VALUE with type t = float = Stdlib.Float

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
