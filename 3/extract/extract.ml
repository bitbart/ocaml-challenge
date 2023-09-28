let rec extract i = function
    [] -> failwith "index out of bounds"
  | x::l -> if i=0 then (x,l)
             else let (y,l') = extract (i-1) l
                  in (y, x::l')
;;

extract 0 [1;2;3];;
extract 1 [1;2;3];;
extract 2 [1;2;3];;
extract 3 [1;2;3];;
