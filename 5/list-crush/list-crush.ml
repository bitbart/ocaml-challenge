let crush1 l =
  let rec crush1_rec z l = match l with
      [] -> []
    | [x] -> if x=z then [] else [x]
    | x::y::l' -> if x=z
                  then crush1_rec z (y::l')
                  else if x=y then crush1_rec y l'
                  else x::(crush1_rec x (y::l'))
  in match l with
       [] -> []
     | [x] -> [x]
     | x::y::l' -> if x=y then crush1_rec x (y::l') else x::(crush1_rec x (y::l'))
;;

let rec crush l =
  let l1 = crush1 l in
  if List.length l = List.length l1 then l1
  else crush l1;;

crush1 [1;2;3];;
crush1 [1;1;2;1;1;3];;
crush1 [1;1;2;1;1;1;3;3;2];;

crush [1;1;2;1;1;3;2;1;1;1;3;3;1;1;2;3];;
crush [1;1;2;1;1;1;3;3;2];;
