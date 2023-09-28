let rec head n = function
    [] -> []
  | x::l -> if n=0 then [] else x::(head (n-1) l)
;;

head 3 [1;2;3;4;5];;

let rec split i = function
    [] -> ([],[])
  | x::l -> if i=0 then ([],x::l)
            else let (l1,l2) = split (i-1) l in (x::l1,l2)
;;

split 3 [1;2;3;4;5];;

let split2 i j l =
  let (l0,r) = split i l in
  let (l1,l2) = split (j-i) r in
  (l0,l1,l2)
;;

split2 2 5 [1;2;3;4;5;6;7];;

let rec exp k l = if k=0 then [] else l @ (exp (k-1) l);;

let rec pump k i j l = 
  assert (i<=j && j<List.length l);
  let (l0,l1,l2) = split2 i j l
  in l0 @ (exp k l1) @ l2;;

pump 5 2 4 [1;2;3;4;5;6;7];;
