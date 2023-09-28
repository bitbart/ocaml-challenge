(* in_range : 'a -> 'a -> 'a -> bool *)

let in_range x a b = (x >= a && x <= b);;

in_range (5) 0 10;;
in_range (-4) 0 10;;

(* in_range : int -> int -> int -> bool *)
let in_range (x:int) a b = (x >= a && x <= b);;
