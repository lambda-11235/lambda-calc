
let id = \x. x
let const = \x y. x
let flip = \f x y. f y x
let Y = \f. (\x. x x) (\x. f (x x))
let bot = Y id

let true = \x y. x
let false = \x y. y
let not = \b. b false true
let and = \x y. x y x
let or = \x y. x x y

let pair = \x y f. f x y
let fst = \p. p (\x y. x)
let snd = \p. p (\x y. y)

let injL = \x f g. f x
let injR = \y f g. g y
