let rec rev = function
    [] -> []
  | x::l -> (rev l)@[x]
;;
