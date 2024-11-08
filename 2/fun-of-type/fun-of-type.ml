(* f1 : int -> bool *)

let f1 x = x>=0;;

(* f2 : bool -> int *)

let f2 b = if b then 0 else 1;;

(* f3 : int -> (int * bool) *)

let f3 x = (x+1,true);;

(* f4 : (int * bool) -> int *)

let f4 (x,b) = if b then x+1 else x;;

(* f5 : int -> (int -> int) *)

let f5 = fun x -> fun y -> x+y;;

(* f6 : int -> (int -> bool) *)

let f6 = fun x -> fun y -> x>y;;

(* f7 : bool -> (int -> bool) *)

let f7 = fun b -> fun x -> b && x>0;;

(* f8 : bool -> (bool -> int) *)

let f8 = fun b1 -> fun b2 -> if b1 && b2 then 1 else 0;;

(* f9 : bool -> (int -> int) *)

let f9 = fun b -> if b then fun x -> x+1 else fun x -> x-1;;

(* f10 : (int -> int) -> int *)

let f10 f = f 0 + 1;;

(* f11 : (int -> bool) -> int *)

let f11 f = if f 0 then 0 else 1;;

(* f12 : (bool -> int) -> int *)

let f12 f = f true + 1;;

(* f13 : (int -> bool) -> bool *)

let f13 f = f 0 && f 1;;

(* f14 : (bool -> bool) -> int *)

let f14 f = if f true then 0 else 1;;

(* f15 : int -> (int * int) -> int *)

let f15 x (y,z) = x + y + z;;

(* f16 : int -> (int -> (int -> int)) *)

let f16 x = fun y -> fun z -> x + y + z;;

(* f17 : (int -> int) -> (int -> int) *)

let f17 f = fun x -> f (x+1) + 1;;

(* f18 : ((int -> int) -> int) -> int *)

let f18 f = (f (fun x -> x+1)) + 1;;

(* f19 : (int -> int) -> (bool -> bool) *)

let f19 f = fun b -> b && (f 0 > 0);;

(* f20 : (int -> bool) -> (bool -> int) *)

let f20 f = fun b -> if b && f 0 then 0 else 1;;
