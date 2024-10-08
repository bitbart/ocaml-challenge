type nat = Z | S of nat

(************************************************************
                                 f(x) = b
 -----------   ---------------   ------------
 f(Z) = true   f(S(Z)) = false   f(S S x) = b
 ************************************************************)

let rec is_even = function
    Z -> true
  | S(S(x)) -> is_even x
  | _ -> false
;;

is_even (S(S(S(S(Z)))));;


(************************************************************
                          f(x) = n
 --------   -----------   ---------------
 f(Z) = 0   f(S(Z)) = 0   f(S S x) = S(n)
 ************************************************************)

let rec halve = function
    Z -> Z
  | S(Z) -> Z
  | S(S(x)) -> let n = halve x in S(n)
;;

let nl = [Z;S(Z);S(S(Z));S(S(S(Z)));S(S(S(S(Z))));S(S(S(S(S(Z)))));S(S(S(S(S(S(Z))))));S(S(S(S(S(S(S(Z)))))))];;
List.map halve nl;;


(************************************************************
              f(a,b') = n       
 ----------   ------------------
 f(a,Z) = a   f(a,S(b')) = S(n) 
 ************************************************************)

let rec add a b = match b with
    Z -> a
  | S(b') -> S(add a b')
;;

add (S(S(Z))) (S(S(Z)));;


(************************************************************
              f(a,b') = n       
 ----------   --------------------
 f(a,Z) = Z   f(a,S(b')) = add a n
 ************************************************************)

let rec mul a b = match b with
    Z -> Z
  | S(b') -> add a (mul a b')
;;

mul (S(S(Z))) (S(S(S(S(Z)))));;


(************************************************************
              b <> Z           f(a,b) = v
 ----------   --------------   ----------------
 f(Z,Z) = Z   f(Z,b) = false   f(S(a),S(b)) = v
 ************************************************************)

let rec equals a b = match (a,b) with
    (Z,Z) -> true
  | (Z,_) | (_,Z) -> false
  | (S(a'),S(b')) -> equals a' b';;

equals (S(S(Z))) (S(S(Z)));;


(************************************************************
                 a <> Z           f(a,b) = v
 -------------   --------------   ----------------
 f(Z,b) = true   f(a,Z) = false   f(S(a),S(b)) = v
 ************************************************************)

let rec leq a b = match (a,b) with
    (Z,_) -> true
  | (_,Z) -> false
  | (S(a'),S(b')) -> leq a' b';;

leq (S(S(Z))) (S(S(S(Z))));;
