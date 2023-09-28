type card = Joker | Val of int;;

let card_ok = function
    Joker -> true
  | Val n -> n>=1 && n<=10
;;

let win player dealer =
  assert (card_ok player && card_ok dealer);
  match (player,dealer) with
    (_,Joker) -> false
  | (Joker,_) -> true
  | (Val p, Val d) -> (p > d);;


assert(win Joker (Val 7) = true);;
assert(win (Val 7) Joker = false);;
assert(win Joker Joker = false);;
assert(win (Val 9) (Val 7) = true);;
assert(win (Val 7) (Val 7) = false);;
