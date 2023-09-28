let tris = function
    (_,x,y,z) when x=y && y=z -> true
  | (x,_,y,z) when x=y && y=z -> true
  | (x,y,_,z) when x=y && y=z -> true
  | (x,y,z,_) when x=y && y=z -> true   
  | _ -> false
;;

let hand () =
  (1 + Random.int 10,
   1 + Random.int 10,
   1 + Random.int 10,
   1 + Random.int 10)
;;

tris (hand());;
