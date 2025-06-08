module type VALUE = sig
  type t

  val to_string : t -> string
end

module type EQ_VALUE = sig
  include VALUE

  val equal : t -> t -> bool
end

module type CMP_VALUE = sig
  include EQ_VALUE

  val compare : t -> t -> int
end

module AsEq (V : VALUE) : EQ_VALUE with type t = V.t = struct
  include V

  let equal = ( = )
end

module AsCmp (V : VALUE) : CMP_VALUE with type t = V.t = struct
  include V

  let equal = ( = )

  let compare = Stdlib.compare
end
