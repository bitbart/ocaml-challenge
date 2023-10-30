let is_even x = (x mod 2 = 0);;

let hand_ok n = (n>=1 && n<=5);;

let win a b = match (a,b) with
    (a,b) when not (hand_ok a) && not (hand_ok b) -> 0  (* no one wins *)
  | (a,b) when not (hand_ok b) -> 1                    (* a wins *)
  | (a,b) when not (hand_ok a) -> -1                   (* b wins *)
  | (a,b) -> if (is_even (a+b)) then 1 else -1
;;

let win2 a b = 
  let x = (a >= 1 && a <= 5) in
  let y = (b >= 1 && b <= 5) in
  if (x && y) then
    if ((a+b) mod 2 = 0) then 1
    else -1
  else if (x && not y) then 1
  else if (not x && y) then -1
  else 0;;

Random.self_init ();;

let rec test n =
  if n=0 then string_of_int (Random.int(7))
  else let t1 = test (n-1) and t2 = test (n-1) in
    match Random.int(4) with
    | 0 -> "(" ^ t1 ^ "+" ^ t2 ^ ")"
    | 1 -> "(" ^ t1 ^ "-" ^ t2 ^ ")"
    | 2 -> "(" ^ t1 ^ "*" ^ t2 ^ ")"           
    | _ -> "win (" ^ t1 ^ " mod 6)(" ^ t2 ^ " mod 6)"
;;

let testwin n = "win(" ^ test n ^ " mod 7)(" ^ test n ^ " mod 7)";;

(* true (0) *)
assert(win(0)(6)=0);;

(* false (1) *) 
assert(win(((2*0)+(1*3)) mod 7)(((0+6)-(6*4)) mod 7)=0);;

(* false (-1) *)
assert(win(win (win ((0*3) mod 6)((2+5)  mod 6) mod 6)(((1+4)-win (1 mod 6)(6 mod 6)) mod 6) mod 7)(win (((0*5)+(5+0)) mod 6)(((2-0)+win (2 mod 6)(6  mod 6)) mod 6) mod 7)=1);;

(* true (-1) *)
assert(win((((win (0 mod 6)(3 mod 6)+(1+4))+((5+0)+win (1 mod 6)(6 mod 6)))*(((6+4)+(1-6))*((2+5)-(5+0)))) mod 7)(((((2*2)+(3+4))+(win (4 mod 6)(3 mod 6)+(1+2)))-win (((2-3)*win (3 mod 6)(4 mod 6)) mod 6)(((2-4)-win (5 mod 6)(2 mod 6)) mod 6)) mod 7) = -1);;

(* true (1) *)
assert(win(((((4*3)*win (3 mod 6)(1 mod 6))+win ((3-5) mod 6)(win (4 mod 6)(1 mod 6) mod 6))-((win (0 mod 6)(3 mod 6)-(4-0))-((6-6)-win (5 mod 6)(5 mod 6)))) mod 7)((win ((win (5 mod 6)(6 mod 6)*(2-6)) mod 6)(((2*4)+win (6 mod 6)(5 mod 6)) mod 6)*(win ((1-0) mod 6)((6-5) mod 6)+((3+4)-(0*5)))) mod 7)=1);;

(* true (0) *)
assert(win((win (((1-3)*(4*4)))(((1-2)*(2+2)))-(((5*4)-(4*3))-win ((6+3))((6*3)))) mod 6)((((win (2)(1)+(3-3))+win ((5*3))((3-1)))+(((3*3)-(3*1))*(win (2)(4)-(5+4)))) mod 6)=0);;

testwin 4;;
