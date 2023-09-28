let is_posfrac (a,b) =
  (a>=0 && b>0) || (a<=0 && b<0)
;;                 

(*
compare_frac (a1,b1) (a2,b2) =
0  if a1/b1 = a2/b2
1  if a1/b1 > a2/b2
-1 if a1/b1 < a2/b2
*)

let compare_posfrac (a1,b1) (a2,b2) =
  assert (is_posfrac (a1,b1) && is_posfrac (a2,b2));
  let l = a1 * b2 and r = a2 * b1 in
  if l=r then 0
  else if l>r then 1
  else -1
;;

compare_posfrac (1,2) (2,1);;

assert (compare_posfrac (1,2) (2,4) == 0);;
assert (compare_posfrac (1,2) (1,3) == 1);;
assert (compare_posfrac (1,2) (2,3) == -1);;

let compare_frac (a1,b1) (a2,b2) =
  match (is_posfrac (a1,b1), is_posfrac (a2,b2)) with
  | (false,false) -> - (compare_posfrac (-a1,b1) (-a2,b2))
  | (false,true) -> -1
  | (true,false) -> 1   
  | (true,true) -> compare_posfrac (a1,b1) (a2,b2)
;;

assert (compare_frac (-1,2) (-2,4) == 0);;
assert (compare_frac (1,2) (1,3) == 1);;
assert (compare_frac (-1,2) (1,3) == -1);;
assert (compare_frac (1,2) (-1,3) == 1);;
assert (compare_frac (-1,2) (-1,3) == -1);;
