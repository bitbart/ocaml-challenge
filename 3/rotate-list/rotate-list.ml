let rec split n = function
    [] -> ([],[])
  | l when n=0 -> ([],l)
  | a::l -> let (l1,l2) = (split (n-1) l) in (a::l1,l2)
;;

split 5 [1;2;3;4];;

let rotate n l =
  assert (n>=0);
  let m = List.length l in
  let (l1,l2) = split (n mod m) l in l2 @ l1;;

assert(rotate 0 [5;6;7;8] = [5;6;7;8]);;  
assert(rotate 1 [5;6;7;8] = [6;7;8;5]);;  
assert(rotate 2 [5;6;7;8] = [7;8;5;6]);;  
assert(rotate 3 [5;6;7;8] = [8;5;6;7]);;  
assert(rotate 4 [5;6;7;8] = [5;6;7;8]);;  
