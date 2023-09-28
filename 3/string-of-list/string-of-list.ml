let string_of_list l =
    let rec string_of_list_rec = function
    [] -> ""
  | [x] -> (string_of_int x)
  | x::l -> (string_of_int x) ^ ";" ^ (string_of_list_rec l)
in "[" ^ (string_of_list_rec l) ^ "]"
;;

assert(string_of_list [] = "[]");;
assert(string_of_list [1] = "[1]");;
assert(string_of_list [1;2] = "[1;2]");;
assert(string_of_list [1;2;3] = "[1;2;3]");;
