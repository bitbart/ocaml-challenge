let mem x l = List.fold_left (fun b y -> b || x=y) false l;;

mem 2 [1;2;3];;

let mkset l = List.fold_left (fun l' x -> if mem x l' then l' else x::l') [] l;;

mkset [1;2;3;1;2;1];;

let count x l = List.fold_left (fun n y -> n + (if x=y then 1 else 0)) 0 l;;

count 1 [1;2;1;4;5;1;6];;

let dup l = List.length l <> List.length (mkset l);;

let dup2 l = not (List.for_all (fun n -> n=1) (List.map (fun x -> count x l) (mkset l)));;

dup [1;2;3;4;5];;
dup2 [1;2;3;4;5];;
dup [1;2;3;4;2;5];;
dup2 [1;2;3;4;2;5];;

let union xl yl = List.fold_left (fun ul y -> if mem y xl then ul else y::ul) xl yl;;

union [1;3;5] [2;3;4];;

let inter xl yl = List.fold_left (fun il y -> if mem y xl then y::il else il) [] yl;;

inter [1;5;7;2] [2;3;4;1];;

let diff xl yl = List.fold_left (fun dl x -> if mem x yl then dl else x::dl) [] xl;;

diff [1;2;3;4;5] [1;4];;

let dsum xl yl = union (List.map (fun x -> (0,x)) xl) (List.map (fun y -> (1,y)) yl);;

dsum [1;2] [2;3];;
