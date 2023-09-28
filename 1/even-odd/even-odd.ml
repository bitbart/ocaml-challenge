let is_even x = (x mod 2 = 0);;

let hand_ok n = (n>=1 && n<=5);;

let win a b = match (a,b) with
    (a,b) when not (hand_ok a) && not (hand_ok b) -> 0  (* no one wins *)
  | (a,b) when not (hand_ok b) -> 1                    (* a wins *)
  | (a,b) when not (hand_ok a) -> -1                   (* b wins *)
  | (a,b) -> if (is_even (a+b)) then 1 else -1
;;
