type grade = Val of int | CumLaude;;

let is_valid x = match x with
    Val v -> v>=18 && v<=30
  | CumLaude -> true
;;

let int_of_grade x = 
  match x with 
    Val v when v>=18 && v<=30 -> v
  | CumLaude -> 32
  | _ -> failwith "invalid grade"
;;

let avg l = (float_of_int (List.fold_right (+) (List.map int_of_grade l) 0)) /. (float_of_int (List.length l));;

assert(avg [(Val 18);CumLaude;(Val 22)] = 24.);;
