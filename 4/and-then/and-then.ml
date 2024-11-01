let ( -?-> ) (o : 'a option) (next : 'a -> 'b option) : 'b option =
  match o with
  | None -> None
  | Some x -> next x
;;

(* Unit tests *)
let succ_opt n = Some (n+1);;
assert(None -?-> succ_opt = None);;
assert(Some 3 -?-> succ_opt = Some 4);;

assert(
  (Some 1 -?-> fun x ->
    Some 2 -?-> fun y ->
      Some 3 -?-> fun z ->
        Some (x + y + z))
  = Some 6);;

assert(
  (Some 1 -?-> fun x ->
    None   -?-> fun y ->
      Some 3 -?-> fun z ->
        Some (x + y + z))
  = None);;

List.nth_opt;;

let first_third_fifth l =
  List.nth_opt l 0 -?-> fun x -> 
  List.nth_opt l 2 -?-> fun y ->
  List.nth_opt l 4 -?-> fun z ->
  Some (x,y,z)
;;

first_third_fifth [1;2;3;4];;
first_third_fifth [1;2;3;4;5;6];;
