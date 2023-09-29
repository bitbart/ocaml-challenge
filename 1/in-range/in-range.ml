(* in_range : 'a -> 'a -> 'a -> bool *)

let in_range x a b = (x >= a && x <= b);;

assert(in_range (5) 0 10);;
assert(not (in_range (-4) 0 10));;

(* in_range2 : int -> int -> int -> bool *)
let in_range2 (x:int) a b = (x >= a && x <= b);;

assert(in_range2 (5) 0 10);;
assert(not (in_range2 (-4) 0 10));;
