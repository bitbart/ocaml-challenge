let nand p q = match (p,q) with
    (true,true) -> false
  | _ -> true
;;

Random.self_init ();;

let rec test n =
  if n=0 then 
    let b = Random.int(2) in
    if b=0 then "true" else "false"
  else let t1 = test (n-1) and t2 = test (n-1) in
    match Random.int(4) with
    | 0 -> "not(" ^ t1 ^ ")"
    | 1 -> "(" ^ t1 ^ "&&" ^ t2 ^ ")"
    | 2 -> "(" ^ t1 ^ "||" ^ t2 ^ ")"
    | _ -> "nand(" ^ t1 ^ ")(" ^ t2 ^ ")"
;;

let testnand n = "nand(" ^ test n ^ ")(" ^ test n ^ ")";;

(* true *)
assert(nand(not((true&&true)))(nand(not(false))(nand(false)(true))));;

(* true *)
assert(nand((not(nand(false)(false))&&(not(false)&&(false&&false))))(not(((false&&true)&&not(false)))));;

(* false *)
assert(nand(not(((nand(true)(true)||(false||true))&&(nand(false)(false)&&(true&&false)))))(not((nand(not(false))((true||false))||nand((true&&true))(nand(false)(true))))));;

(* false *)
assert(nand(not(((nand(not(false))(not(true))&&not((true||true)))&&(not((false&&false))&&((true&&true)||nand(true)(false))))))(nand((nand(not((true||false)))(((true||false)||(false&&false)))&&(((true||false)&&(true&&false))||not(nand(true)(false)))))(not(nand(not(nand(true)(false)))(not(not(false)))))));;

(* true *)
assert(nand(not(((nand((false&&true))((true&&false))||not(nand(true)(false))))||((not((true&&true))||nand((true&&false))((true||false)))||((not(false)||(false&&true))||(not(true)||not(true))))))(not(nand(not(nand(not(not(true)))(nand(nand(false)(true))((false&&false)))))(nand((nand(not(true))(nand(true)(true))||(nand(true)(true)&&(false&&true))))((nand((false&&true))(nand(true)(true))||(not(true)||(true||true))))))));;
  
print_string(testnand 2);;
