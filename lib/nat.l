
let zero = \f x. x
let succ = \n f x. f (n f x)

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

let add = \n m. n succ m
let mult = \n m. n (add m) zero
let power = \n m. m (mult n) one

# pred zero = zero, pred (succ n) = n
let pred = \n.
  snd (n (\p. p (\next. \prev. pair (succ next) next))
        (pair zero zero))

# sub n m = if n >= m then n - m else zero
let sub = \n m. m pred n


# n = 0, n > 0, n > m, n < m, and n = m, respectively.
let isZero = \n. n (const false) true
let isSucc = \n. n (const true) false
let greaterThan = \n. \m. isSucc (sub n m)
let lessThan = \n. \m. greaterThan m n
let equal = \n. \m. not (or (greaterThan n m) (lessThan n m))


# Returns a pair of the quotient and remainder, respectively, of the division of
# n by m. Produce incorrect result when m = 0.
let divRem = \n. \m.
  n (\p. p (\q. \r.
      (lessThan (succ r) m) (pair q (succ r)) (pair (succ q) zero)))
    (pair zero zero)


let fact = Y (\f. \n. (isZero n) one (mult n (f (pred n))))
