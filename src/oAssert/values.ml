module type VALUE = sig
  type t

  val to_string : t -> string
end

module type EQUATABLE_VALUE = sig
  include VALUE

  val equal : t -> t -> bool
end

module type COMPARABLE_VALUE = sig
  include EQUATABLE_VALUE

  val compare : t -> t -> int
end

module AsEquatable (V : VALUE) : EQUATABLE_VALUE with type t = V.t = struct
  include V

  let equal = ( = )
end

module AsComparable (V : VALUE) : COMPARABLE_VALUE with type t = V.t = struct
  include V

  let equal = ( = )

  let compare = Stdlib.compare
end

module Bool : COMPARABLE_VALUE with type t = bool = Stdlib.Bool

module Int : COMPARABLE_VALUE with type t = int = Stdlib.Int

module Float : COMPARABLE_VALUE with type t = float = Stdlib.Float

module String : COMPARABLE_VALUE with type t = string = struct
  include Stdlib.String

  let to_string s = s
end
