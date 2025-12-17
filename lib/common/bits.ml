module Bit = struct
  type t = O | I

  let to_string = function
    | O -> "0"
    | I -> "1"

  let to_int = function
    | O -> 0
    | I -> 1

  let from_int n =
    if n = 0 then O
    else if n = 1 then I
    else raise (Failure "Can not convert number other than 0 or 1 to bit")
end


module BitVec : sig
  type t = Bit.t list
  val to_int : t -> int
  val from_int : int -> t
  val zext : t -> int -> t
end = struct
  type t = Bit.t list
  (* Least significant bit is the head *)

  let rec to_int (bv:t) =
    match bv with
    | [] -> raise (Failure "Can not convert empty list to int")
    | [O] -> 0
    | [I] -> 1
    | b :: bv' -> (Bit.to_int b) + 2 * (to_int bv')

  let rec from_int n =
    if n < 0
    then raise (Failure "Can not convert negative integer to bitvector")
    else if n < 2 then [Bit.from_int n]
    else
      let b = Bit.from_int (n mod 2) in
      let bv = from_int (n / 2) in
      b :: bv

  let zext bv n =
    match bv with
    | [] -> raise (Failure "Can not extend empty bitvector")
    | _ ->
      if n < 0 then raise (Failure "Invalid negative extension argument")
      else
        let l = List.length bv in
        let delta = n - l in
        if delta <= 0 then bv
        else
          let zeroes = List.init delta (fun _ -> Bit.O) in
          bv @ zeroes
end


module Bits2 : sig
  type t = Bit.t * Bit.t
  val to_bitvec : t -> BitVec.t
  val from_bitvec : BitVec.t -> t
  val to_int : t -> int
  val from_int : int -> t
  val from_int_mod : int -> t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val lognot : t -> t
  val shift_left : t -> t -> t
  val shift_right : t -> t -> t
  val compare : t -> t -> int
  val to_string : t -> string
end = struct
  type t = Bit.t * Bit.t
  let length = 2
  let modulus = Float.to_int (Float.exp2 (Float.of_int length))
  (* Least significant bit is the first element *)

  let to_bitvec bits2 =
    let b0, b1 = bits2 in
    [b0; b1]

  let to_int bits2 =
    BitVec.to_int (to_bitvec bits2)

  let from_bitvec bv2 =
    let bv2' = BitVec.zext bv2 length in
    match bv2' with
    | [b0; b1] -> (b0, b1)
    | _ -> raise (Failure "Argument too big to fit in 2 bits")

  let from_int n =
    from_bitvec (BitVec.from_int n)

  let from_int_mod n =
    let n' = n mod modulus in
    from_int n'

  let zero = from_int 0

  let lift2 f x y =
    from_int_mod (f (to_int x) (to_int y))

  let lift1 f x =
    from_int_mod (f (to_int x))

  let add = lift2 (+)
  let sub = lift2 (-)
  let succ = lift1 (Int.succ)
  let pred = lift1 (Int.pred)

  let logand = lift2 (Int.logand)
  let logor = lift2 (Int.logor)
  let lognot = lift1 (Int.lognot)
  let shift_left = lift2 (Int.shift_left)
  let shift_right = lift2 (Int.shift_right)

  let compare b1 b2 =
    Int.compare (to_int b1) (to_int b2)

  let to_string b =
    let (b0, b1) = b in
    let s0 = Bit.to_string b0 in
    let s1 = Bit.to_string b1 in
    s1 ^ s0
end



module Bits4 : sig
  type t = Bit.t * Bit.t * Bit.t * Bit.t
  val to_bitvec : t -> BitVec.t
  val from_bitvec : BitVec.t -> t
  val to_int : t -> int
  val from_int : int -> t
  val from_int_mod : int -> t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val lognot : t -> t
  val shift_left : t -> t -> t
  val shift_right : t -> t -> t
  val compare : t -> t -> int
  val to_string : t -> string
end = struct
  type t = Bit.t * Bit.t * Bit.t * Bit.t
  let length = 4
  let modulus = Float.to_int (Float.exp2 (Float.of_int length))
  (* Least significant bit is the first element *)

  let to_bitvec bits4 =
    let b0, b1, b2, b3 = bits4 in
    [b0; b1; b2; b3]

  let to_int bits4 =
    BitVec.to_int (to_bitvec bits4)

  let from_bitvec bv4 =
    let bv4' = BitVec.zext bv4 length in
    match bv4' with
    | [b0; b1; b2; b3] -> (b0, b1, b2, b3)
    | _ -> raise (Failure "Argument too big to fit in 4 bits")

  let from_int n =
    from_bitvec (BitVec.from_int n)

  let from_int_mod n =
    let n' = n mod modulus in
    from_int n'

  let zero = from_int 0

  let lift2 f x y =
    from_int_mod (f (to_int x) (to_int y))

  let lift1 f x =
    from_int_mod (f (to_int x))

  let add = lift2 (+)
  let sub = lift2 (-)
  let succ = lift1 (Int.succ)
  let pred = lift1 (Int.pred)

  let logand = lift2 (Int.logand)
  let logor = lift2 (Int.logor)
  let lognot = lift1 (Int.lognot)
  let shift_left = lift2 (Int.shift_left)
  let shift_right = lift2 (Int.shift_right)

  let compare b1 b2 =
    Int.compare (to_int b1) (to_int b2)

  let to_string b =
    let (b0, b1, b2, b3) = b in
    let s0 = Bit.to_string b0 in
    let s1 = Bit.to_string b1 in
    let s2 = Bit.to_string b2 in
    let s3 = Bit.to_string b3 in
    s3 ^ s2 ^ s1 ^ s0
end


module Bits8 : sig
  type t = Bit.t * Bit.t * Bit.t * Bit.t * Bit.t * Bit.t * Bit.t * Bit.t
  val to_bitvec : t -> BitVec.t
  val from_bitvec : BitVec.t -> t
  val to_int : t -> int
  val from_int : int -> t
  val from_int_mod : int -> t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val lognot : t -> t
  val shift_left : t -> t -> t
  val shift_right : t -> t -> t
  val compare : t -> t -> int
  val to_string : t -> string

end = struct
  type t = Bit.t * Bit.t * Bit.t * Bit.t * Bit.t * Bit.t * Bit.t * Bit.t
  let length = 8
  let modulus = Float.to_int (Float.exp2 (Float.of_int length))
  (* Least significant bit is the first element *)

  let to_bitvec bits8 =
    let b0, b1, b2, b3, b4, b5, b6, b7 = bits8 in
    [b0; b1; b2; b3; b4; b5; b6; b7]

  let to_int bits8 =
    BitVec.to_int (to_bitvec bits8)

  let from_bitvec bv8 =
    let bv8' = BitVec.zext bv8 length in
    match bv8' with
    | [b0; b1; b2; b3; b4; b5; b6; b7] -> (b0, b1, b2, b3, b4, b5, b6, b7)
    | _ -> raise (Failure "Argument too big to fit in 8 bits")

  let from_int n =
    from_bitvec (BitVec.from_int n)

  let from_int_mod n =
    let n' = n mod modulus in
    from_int n'

  let zero = from_int 0

  let lift2 f x y =
    from_int_mod (f (to_int x) (to_int y))

  let lift1 f x =
    from_int_mod (f (to_int x))

  let add = lift2 (+)
  let sub = lift2 (-)
  let succ = lift1 (Int.succ)
  let pred = lift1 (Int.pred)

  let logand = lift2 (Int.logand)
  let logor = lift2 (Int.logor)
  let lognot = lift1 (Int.lognot)
  let shift_left = lift2 (Int.shift_left)
  let shift_right = lift2 (Int.shift_right)

  let compare b1 b2 =
    Int.compare (to_int b1) (to_int b2)

  let to_string b =
    let (b0, b1, b2, b3, b4, b5, b6, b7) = b in
    let s0 = Bit.to_string b0 in
    let s1 = Bit.to_string b1 in
    let s2 = Bit.to_string b2 in
    let s3 = Bit.to_string b3 in
    let s4 = Bit.to_string b4 in
    let s5 = Bit.to_string b5 in
    let s6 = Bit.to_string b6 in
    let s7 = Bit.to_string b7 in
    s7 ^ s6 ^ s5 ^ s4 ^ s3 ^ s2 ^ s1 ^ s0
end


let bits4_to_bits8 b4 =
  b4
  |> Bits4.to_int
  |> Bits8.from_int_mod

let bits8_to_bits4 b8 =
  b8
  |> Bits8.to_int
  |> Bits4.from_int_mod

let bits2_to_bits4 b2 =
  b2
  |> Bits2.to_int
  |> Bits4.from_int_mod

let bits4_to_bits2 b4 =
  b4
  |> Bits4.to_int
  |> Bits2.from_int_mod

let bits2_to_bits8 b2 =
  b2
  |> Bits2.to_int
  |> Bits8.from_int_mod

let bits8_to_bits2 b8 =
  b8
  |> Bits8.to_int
  |> Bits2.from_int_mod

let concat_224 b2_a b2_b b4 =
  let b2_a_0, b2_a_1 = b2_a in
  let b2_b_0, b2_b_1 = b2_b in
  let b4_0, b4_1, b4_2, b4_3 = b4 in
  (b2_a_0, b2_a_1, b2_b_0, b2_b_1, b4_0, b4_1, b4_2, b4_3)

let concat_44 b4_a b4_b =
  let b4_a_0, b4_a_1, b4_a_2, b4_a_3 = b4_a in
  let b4_b_0, b4_b_1, b4_b_2, b4_b_3 = b4_b in
  (b4_a_0, b4_a_1, b4_a_2, b4_a_3, b4_b_0, b4_b_1, b4_b_2, b4_b_3)
