(************************************************************
       n>=0     n<0
---   ------   -----
 0    -(n+1)    -n
 ************************************************************)

let rec enum_int = function
    0 -> 0
  | n when n>0 -> let n' = enum_int (n-1) in
    if n'>=0 then -(n'+1) else -n'
  | _ -> failwith "negative argument"
;;

List.map enum_int [0;1;2;3;4;5;6;7;8;9];;
