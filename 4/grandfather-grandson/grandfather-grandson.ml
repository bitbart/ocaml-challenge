let exchange k = 
  (k mod 10)*10 + (k/10);;

assert(exchange 73 = 37);;

let is_valid_answer (grand_father_age, grand_son_age) =
  grand_father_age = 4 * grand_son_age &&
  3 * (exchange grand_father_age) = exchange grand_son_age;;

assert(is_valid_answer (72,18) = true);;
assert(is_valid_answer (88,22) = false);;

let rec find_snd_up answer =
  if is_valid_answer answer then answer
  else if (fst answer = snd answer) then (-1,-1)
  else find_snd_up (fst answer, snd answer + 1);;
    
let rec find answer =
  if is_valid_answer answer then answer
  else let a = find_snd_up answer in
    if a = (-1,-1) then find (fst answer - 1, snd answer)
    else a
;;

find (100,0);;
