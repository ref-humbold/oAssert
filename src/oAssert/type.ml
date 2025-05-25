module type TYPE = sig
  type t

  val to_string : t -> string
end

module type EQUATABLE_TYPE = sig
  include TYPE

  val equal : t -> t -> bool
end

module type COMPARABLE_TYPE = sig
  include EQUATABLE_TYPE

  val compare : t -> t -> int
end

module AsEquatable (T : TYPE) : EQUATABLE_TYPE with type t = T.t = struct
  include T

  let equal = ( = )
end

module AsComparable (T : TYPE) : COMPARABLE_TYPE with type t = T.t = struct
  include T

  let equal = ( = )

  let compare = Stdlib.compare
end

module Int : COMPARABLE_TYPE with type t = int = Stdlib.Int

module Float : COMPARABLE_TYPE with type t = float = Stdlib.Float

module String : COMPARABLE_TYPE with type t = string = struct
  include Stdlib.String

  let to_string s = s
end
