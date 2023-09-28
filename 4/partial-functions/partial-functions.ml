(* is_fun : 'a list -> 'b list -> ('a * 'b) list -> bool *)

let _A0 = [1;2;3];;
let _B0 = [2;3;4];;

let f1 = [(1,2);(2,3);(1,4)]
and f2 = [(1,2);(2,3);(4,4)]
and f3 = [(1,2);(2,3);(3,5)]
and f4 = [(1,2);(2,3);(3,4)]
and f5 = [(1,2);(2,3)]
and f6 = [(1,2);(2,3);(3,2)];;

let rec dup = function
    [] -> false
  | x::l -> List.mem x l

(* is_fun : ('a * 'b) list -> bool *)
(* is_fun _A _B f checks if f is a partial function from A to B *)
              
let is_fun _A _B f =
  List.for_all (fun (a,b) -> List.mem a _A) f &&
  List.for_all (fun (a,b) -> List.mem b _B) f &&
  not (dup (List.map fst f))
;;

assert (not (is_fun _A0 _B0 f1));;
assert (not (is_fun _A0 _B0 f2));;
assert (not (is_fun _A0 _B0 f3));;
assert (is_fun _A0 _B0 f4);;
assert (is_fun _A0 _B0 f5);;
assert (is_fun _A0 _B0 f6);;

(* is_tot : ('a * 'b) list -> bool *)

let is_tot _A _B f =
  is_fun _A _B f &&
  List.for_all (fun a -> List.mem a (List.map fst f)) _A
;;

assert (is_tot _A0 _B0 f4);;
assert (not (is_tot _A0 _B0 f5));;
assert (is_fun _A0 _B0 f6);;

(* is_inj : ('a * 'b) list -> bool *)

let is_inj _A _B f =
  is_fun _A _B f &&
  not (dup (List.map snd f))
;;

assert (is_inj _A0 _B0 f4);;
assert (is_inj _A0 _B0 f5);;
assert (not (is_inj _A0 _B0 f6));;

(* is_surj : ('a * 'b) list -> bool *)

let is_surj _A _B f =
  is_fun _A _B f &&
  List.for_all (fun b -> List.mem b (List.map snd f)) _B
;;

assert (is_surj _A0 _B0 f4);;
assert (not (is_surj _A0 _B0 f5));;
assert (not (is_surj _A0 _B0 f6));;

(* rel_of_fun : 'a list -> ('a -> 'b) -> ('a * 'b) list *)
let rec rel_of_fun _A f = List.map (fun a -> (a, f a)) _A;;

let g1 = function
    1 -> 2
  | 2 -> 3
  | 3 -> 5
  | _ -> failwith "undefined"
;;

rel_of_fun [1;2;3] g1;;

let bot = fun x -> failwith "undefined";;

(* bind : ('a -> 'b) -> 'a -> 'b -> 'a -> 'b *)
let bind f a b = fun x -> if x=a then b else f x;;

(* fun_of_rel : ('a * 'b) list -> ('a -> 'b) *) 
(* fun_of_rel f converts the partial function f (represented as a list of pairs)
   into an Ocaml function *)

let rec fun_of_rel = function
    [] -> bot
  | (a,b)::l -> let f = fun_of_rel l in bind f a b;;
 
let g3 = fun_of_rel f3;;
assert (g3 1 = 2);;
assert (g3 2 = 3);;
assert (g3 3 = 5);;
  
(* is_mono : ('a * 'b) list -> bool *)

let rec is_inc = function
    [] -> true
  | [x] -> true
  | x::y::l -> x<=y && is_inc (y::l)
;;

let rec is_dec = function
    [] -> true
  | [x] -> true
  | x::y::l -> x>=y && is_dec (y::l)
;;

let is_mono f =
  let l' = List.map snd (List.sort (fun (a1,b1) (a2,b2) -> compare a1 a2) f)
  in is_inc l' || is_dec l'
;;

assert (is_mono f4);;
assert (is_mono f5);;
assert (not (is_mono f6));;
