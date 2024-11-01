let first_third_fifth = function
  | x1::_::x3::_::x5::_ -> Some (x1,x3,x5) 
  | _ -> None                            
;;

assert(first_third_fifth ["cat"; "dog"] = None);;
assert(first_third_fifth [1; 2; 3; 4; 5; 6] = Some (1, 3, 5));;


