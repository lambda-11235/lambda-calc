
# An inductive natural number encoding. These natural numbers may be though of as
# having the recursive type `rec nat. (nat -> r) -> r -> r`, and closely mimic
# ADT definitions of natural numbers. They are thus easier to reason about in
# recursive algorithms than Church encoded numbers, although they are less
# efficient.

let zero = \f x. x
let succ = \n f x. f n

let isZero = \n. n (const false) true
let isSucc = \n. not (isZero n)

# pred zero = zero, pred (succ n) = n
let pred = \n. n id zero

# Converts numbers to and from their Church encoding.
let toChurch = Y (\rec n. n (\m f x. f ((rec m) f x)) (\f x. x))
let fromChurch = \n. n succ zero

let one   = succ zero
let two   = succ one
let three = succ two
let four  = succ three
let five  = succ four
let six   = succ five
let seven = succ six
let eight = succ seven
let nine  = succ eight
let ten   = succ nine
let inf   = Y (\rec. succ rec)

let add = Y (\rec n m. n (\x. succ (rec x m)) m)
let mult = Y (\rec n m. n (\x. add m (rec x m)) zero)
let power = Y (\rec n m. m (\x. mult n (rec n x)) one)

let sub = Y (\rec n m. m (\x. rec (pred n) x) n)


let lessThan = \n m. isSucc (sub m n)
let greaterThan = flip lessThan
let equal = \n m. not (or (lessThan n m) (greaterThan n m))


# Returns a pair of the quotient and remainder, respectively, of the division of
# n by m. Produces incorrect result when m = 0.
let divRem = Y (\rec n m. n
  (\x. (rec x m) (\q r. (lessThan (succ r) m) (pair q (succ r)) (pair (succ q) zero)))
  (pair zero zero))


let fact = Y (\f. \n. (isZero n) one (mult n (f (pred n))))
