(* L1 = { w in {0,1}* | w = x 0 y => |y|_0 = 0 } *)

let rec lang1 = function
    [] -> true
  | 0::l -> not (List.mem 0 l) && lang1 l
  | 1::l -> lang1 l
  | _ -> false
;;

lang1 [1;1;1;1;1;1;1;1;1;1;1;1];;
lang1 [1;1;1;1;1;1;1;1;1;1;1;0];;
lang1 [1;1;1;1;1;1;1;1;1;1;0;0];;
lang1 [1;1;1;1;1;0;1;0;1;0;1;1];;
lang1 [1;0;0;1;1;1;1;1;1;1;0;1];;

(* L2 = { w in {0,1,2}* | w = x 1 y => |y|_0 = 0 } *)

let rec lang2 = function
    [] -> true
  | 0::l
  | 2::l -> lang2 l             
  | 1::l -> not (List.mem 0 l) && lang2 l
  | _ -> false
;;

lang2 [0;2;0;2;2;0;1;2;2;1;2;1];;
lang2 [0;2;0;2;2;0;1;2;2;1;2;1;0];;
lang2 [1;2;0;1;2;1;1;2;1;2;1;2;2;1;1;2;1;2;1;2;2;1;2;1;2;1;1;2];;

(* L3 = { w | w = x 0 y => exists y' . y = 1 1 y' } *)

let rec lang3 = function
    [] -> true
  | 0::1::1::l -> lang3 l
  | 1::l -> lang3 l
  | _ -> false
;;

lang3 [1;1;1;1;1;1;1;1;1;1;1;1];;
lang3 [1;1;1;1;0;1;0;1;1;1;1;1];;
lang3 [1;1;1;1;0;1;1;0;1;1;1;1];;
lang3 [0;1;1;0;1;1;0;1;1;1;1;1];;
lang3 [0;1;0;1;1;0;1;1;1;1;1;1];;


(* L4 = { w in {0,1}* | |w|_0 >= |w|_1 } *)

let rec count x l = List.fold_left (fun c y -> if y=x then c+1 else c) 0 l;;

let lang4 l = let n0 = count 0 l and n1 = count 1 l in
  n0 >= n1 && n0+n1 = List.length l
;;

lang4 [1;1;1;0;0;0];;
lang4 [0;1;1;1;0;0;0];;
lang4 [1;1;1;1;0;0;0];;
lang4 [1;1;1;2;0;0;0];;
lang4 [1;0;1;1;0;1;0;0;0];;

(* L5 = { w in {0,1}* | |w|_0 = |w|_1 } *)

let lang5 l = let n0 = count 0 l and n1 = count 1 l in
  n0 = n1 && n0+n1 = List.length l
;;

(* L6 = { 0^n 1^n | n >= 0 } *)

let rec lang6_0 n = function
    [] -> n=0
  | 0::l -> lang6_0 (n+1) l
  | 1::l -> lang6_1 (n-1) l
  | _ -> false
and lang6_1 n = function
    [] -> n=0
  | 1::l -> lang6_1 (n-1) l
  | _ -> false
;;

let lang6 l = lang6_0 0 l;;

lang6 [0;1];;
lang6 [0;0;0;1;1;1];;
lang6 [0;0;0;0;1;1;1];;
lang6 [0;0;0;1;1;1;1];;
lang6 [0;0;0;1;1;1;0;1];;


(* L7 = { 0^n 1 0^n | n >= 0 } *)

let rec lang7_asc n = function
    [] -> n=0
  | 0::l -> lang7_asc (n+1) l
  | 1::l -> lang7_dsc n l
  | _ -> false
and lang7_dsc n = function
    [] -> n=0
  | 0::l -> lang7_dsc (n-1) l
  | _ -> false
;;

let lang7 l = lang7_asc 0 l;;

lang7 [1];;
lang7 [0;1;0];;
lang6 [0;0;0;1;0;0;0];;
lang6 [0;0;0;0;0;0;0];;
lang6 [0;0;0;1;0;0];;
lang6 [0;0;1;0;0;0];;


(*** QUIZ ***)

(* L8 = { w in {0,1,2}* | w = x 1 y => |y|_2 = |y|_0 } *)

let rec lang8 =  function
    [] -> true
  | 0::l
  | 2::l -> lang8 l             
  | 1::l -> (count 0 l) = (count 2 l) && lang8 l
  | _ -> false
;;

lang8 [1;2;0];;
lang8 [2;0;2];;
lang8 [1;2;1;0];;
lang8 [1;2;1;0;2;0];;
lang8 [1;2;0;1;0;2;2;0];;

(* false *)
lang8 [1;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;0;2;0;2;1;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;1;2;1;2;1;2];;

(* true *)
lang8 [1;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;0;2;0;2;1;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;0;2;0;2];;

(* true *)
lang8 [2;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;2;2;0;0;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;2;2;0;0;1];;

(* false *)
lang8 [2;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;2;0;0;1;0;2;2;0;1;2;0;0;2;2;0;1;0;0;1;0;0;2;2;1;0;2;2;2;0;0;1;1;1;1;1;1;1];;

(* false *)
lang8 [2;0;0;0;0;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;2;2;0;0;2;2;2;0;0;0;0;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;2;2;0;0;2;2];;


(* L9 = { w | exists w0,w1 : w = w0 w1 and |w0|_0 = |w1|_1 and |w0|_1 = 0 = |w1|_0 *)

let rec lang9_0 n = function
    [] -> n=0
  | 0::l -> lang9_0 (n+1) l
  | 1::l -> lang9_1 (n-1) l
  | 2::l -> lang9_0 n l              
  | _ -> false
and lang9_1 n = function
    [] -> n=0
  | 1::l -> lang9_1 (n-1) l
  | 2::l -> lang9_1 n l              
  | _ -> false
;;

let lang9 l = lang9_0 0 l;;

lang9 [2;0;2;1;2];;
lang9 [0;2;0;1;2];;
lang9 [2;0;1;2;1];;
lang9 [0;0;2;2;1;1;2;2];;

(* false *)
lang9 [0;0;0;0;0;0;2;0;0;0;0;0;2;0;0;0;2;0;0;0;0;0;0;0;0;0;0;0;0;0;2;0;0;0;0;0;0;0;0;0;1;1;1;1;1;1;1;1;1;1;1;2;1;1;1;1;1;1;1;1;2;1;1;1;1;1;1;1;1;1;2;1;1;1;1;1;1;1;1;1];;

(* true *)
lang9 [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1];;

(* false *)
lang9 [2;0;0;0;0;0;2;0;0;0;0;0;0;0;0;0;2;0;0;0;0;0;0;0;0;2;2;0;0;0;0;2;0;0;0;0;0;0;0;0;0;1;1;1;1;1;1;1;1;1;1;1;2;1;2;1;1;1;1;1;1;2;1;1;1;1;1;1;1;1;2;1;1;1;1;1;1;1;1;2];;

(* true *)
lang9 [2;0;0;0;0;0;2;0;0;0;0;0;0;0;0;0;2;0;0;0;0;0;0;0;0;2;2;0;0;0;0;2;0;0;0;0;0;0;0;0;0;1;1;1;1;1;1;1;1;1;1;1;2;1;2;1;1;1;1;1;1;2;1;1;1;1;1;1;1;1;2;1;1;1;1;1;1;1;1;1];;

(* true *)
lang9 [0;0;0;0;0;0;2;0;0;0;0;0;0;0;0;0;2;0;0;0;0;0;0;0;0;0;0;0;0;0;2;0;0;0;0;0;0;0;0;0;1;1;1;1;1;1;1;1;1;1;1;2;1;1;1;1;1;1;1;1;2;1;1;1;1;1;1;1;1;1;2;1;1;1;1;1;1;1;1;1];;