(**********************************************************************
 * mem : 'a -> 'a list -> bool
 **********************************************************************)

let rec mem x = function
    [] -> false
  | y::l -> x=y || mem x l;;

assert(mem 1 [1;3;5]);;
assert(mem 2 [1;3;5] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[1;2]]);;


(**********************************************************************
 * subseteq : 'a list -> 'a list -> bool
 **********************************************************************)

let rec subseteq xl yl = match xl with
    [] -> true
  | x::xl' -> mem x yl && subseteq xl' yl;;

assert(subseteq [] [1;3;5]);;
assert(subseteq [1;5] [5;1]);;
assert(subseteq [1;5] [1;3;5]);;
assert(subseteq [1;5] [5;3;1]);;
assert(subseteq [2] [1;3;5] = false);;
assert(subseteq [[1;2]] [[1];[2];[2;1]] = false);;
assert(subseteq [[1];[2;1]] [[1];[2];[2;1]]);;


(**********************************************************************
 * seteq : 'a list -> 'a list -> bool
 **********************************************************************)

let seteq xl yl = (subseteq xl yl) && (subseteq yl xl);;

assert(seteq [1;5;3] [1;3;5]);;
assert(seteq [1;5;2] [1;3;5] = false);;
assert(seteq [[1;2]] [[2;1]] = false);;
assert(seteq [[1];[1;2]] [[1;2];[1]]);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;


(**********************************************************************
 * dup : 'a list -> bool
 **********************************************************************)

let rec dup = function
    [] -> false
  | x::l -> mem x l || dup l;;

assert(dup [] = false);;
assert(dup [1;1]);;
assert(dup [1;3;5] = false);;
assert(dup [1;3;5;3]);;


(**********************************************************************
 * mkset : 'a list -> 'a list
 **********************************************************************)

let rec mkset = function
    [] -> []
  | x::l -> (if mem x l then [] else [x]) @ mkset l;;

assert(seteq (mkset [1;2;3;2;1]) [1;2;3]);;
assert(seteq (mkset [1;2;1;2;1]) [1;2]);;
assert(seteq (mkset [1;2;3]) [2;3;1]);;


(**********************************************************************
 * union : 'a list -> 'a list -> 'a list
 **********************************************************************)

let rec union xl = function
    [] -> xl
  | y::yl -> (if mem y xl then [] else [y]) @ (union xl yl);;

assert(seteq (union [1;2;3] []) [1;2;3]);;
assert(seteq (union [] [2;3;4]) [2;3;4]);;
assert(seteq (union [1;2;3] [2;3;4]) [1;2;3;4]);;


(**********************************************************************
 * inter : 'a list -> 'a list -> 'a list
 **********************************************************************)

let rec inter xl = function
    [] -> []
  | y::yl -> (if mem y xl then [y] else []) @ (inter xl yl);;


assert(seteq (inter [1;2;3] []) []);;
assert(seteq (inter [] [2;3;4]) []);;
assert(seteq (inter [1;2;3] [2;3;4]) [2;3]);;

(**********************************************************************
 * diff : 'a list -> 'a list -> 'a list
 **********************************************************************)

let rec diff xl yl = match xl with
    [] -> []
  | x::xl' -> (if mem x yl then [] else [x]) @ diff xl' yl;;

assert(seteq (diff [1;2;3] []) [1;2;3]);;
assert(seteq (diff [] [2;3;4]) []);;
assert(seteq (diff [1;2;3] [2;3;4]) [1]);;
assert(seteq (diff [1;2;3] [3;1]) [2]);;

(**********************************************************************
 * dsum : 'a list -> 'a list -> (int * 'a) list
 **********************************************************************)

let dsum xl yl =
  union (List.map (fun x -> (0,x)) xl) (List.map (fun y -> (1,y)) yl);;

assert(seteq (dsum [1;2;3] []) [(0,1);(0,2);(0,3)]);;
assert(seteq (dsum [] [2;3;4]) [(1,2);(1,3);(1,4)]);;
assert(seteq (dsum [1;2] [2;3]) [(0,1);(0,2);(1,2);(1,3)]);;


(**********************************************************************
 * powset : 'a list -> 'a list list
 **********************************************************************)

let rec powset = function
    [] -> [[]]
  | x::xl -> let yll = powset xl in yll @ List.map (fun yl -> union [x] yl) yll;;

assert (powset [] = [[]]);;
assert (seteq (powset [1]) [[];[1]]);;
assert (List.length (powset [1;2]) = 4);;
assert (List.length (powset [1;2;3]) = 8);;
assert (List.length (powset [1;2;3;4]) = 16);;
