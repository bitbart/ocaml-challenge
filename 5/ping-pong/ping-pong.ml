let inv f a b = f b a;;

let alt l =
  let rec alt_rec l comp = match l with
    [] -> true
  | [a] -> true
  | a::b::l' -> if comp a b then alt_rec (b::l') (inv comp) else false
  in match l with
       [] -> true
     | [a] -> true
     | a::b::l' when a<b -> alt_rec (b::l') (>)
     | a::b::l' when b<a -> alt_rec (b::l') (<)
     | _ -> false
;;

alt [1;2;1;3;2;4;5];;

let rec join_ranges = function
    [] -> None
  | [(a,b)] -> Some (a,b)
  | (a,b)::l -> match join_ranges l with
                  None -> None
                | Some (a',b') -> Some (max a a', min b b')
;;

join_ranges [(1,3);(2,4);(1,5)];;

let rec ranges l = match l with
    [] -> []
  | [x] -> []
  | x::y::l' -> (if x<=y then (x,y) else (y,x)) :: (ranges (y::l'))
;;

join_ranges (ranges [1;5;3;8]);;

let has_center r = match r with
    None -> false
  | Some (a,b) -> b-a>1
;;

let rec ping_pong l = alt l && (has_center (join_ranges (ranges l)));; 

ping_pong [1;2;1;3;2;4;0;7];;

let l0 = [1;5;2;5;1;6];;
let l1 = [1;5;2;5;4;3];;
let l2 = [1;5;2;3;2;4];;
let l3 = [1;3;2;4;3;5];;

alt l2;;
ping_pong l2;;
