let rec fib = function
    n when n<0 -> failwith "negative number"    
  | 0 -> []
  | 1 -> [0]
  | 2 -> [0;1]       
  | n -> let l = fib (n-1) in
         match List.rev l with x::y::l' -> l@[x+y]
;;

assert(fib 7 = [0; 1; 1; 2; 3; 5; 8]);;
