let rec mem x = function
    [] -> false
  | y::l -> x=y || mem x l;;

let rec dup = function
    [] -> false
  | x::l -> mem x l || dup l;;

let rec mkset = function
    [] -> []
  | x::l -> (if mem x l then [] else [x]) @ mkset l;;

let rec union xl = function
    [] -> xl
  | y::yl -> (if mem y xl then [] else [y]) @ (union xl yl);;

let rec inter xl = function
    [] -> []
  | y::yl -> (if mem y xl then [y] else []) @ (inter xl yl);;

let rec diff xl yl = match xl with
    [] -> []
  | x::xl' -> (if mem x yl then [] else [x]) @ diff xl' yl;;

let dsum xl yl =
  union (List.map (fun x -> (0,x)) xl) (List.map (fun y -> (1,y)) yl);;

let rec powset = function
    [] -> [[]]
  | x::xl -> let yll = powset xl in yll @ List.map (fun yl -> union [x] yl) yll;;

    
