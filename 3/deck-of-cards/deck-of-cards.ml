type suit = Spades | Hearts | Diamonds | Clubs;;
type card = Card of int * suit;;

let d0 = [Card(1,Spades); Card(1,Hearts); Card(1,Diamonds); Card(1,Clubs)];;

let rec dup l = match l with
    [] -> false
  | c::l' -> List.mem c l' || dup l'
;;

let is_card (Card (n,s)) = n>=1 && n<=10;;

let is_deck l = (not (dup l)) && (List.for_all is_card l);;

is_deck d0;;
is_deck (d0 @ [Card(11,Spades)]);;
is_deck (d0 @ [Card(2,Spades)]);;

let is_complete l = is_deck l && (List.length l = 40);;

let rec range a b = if a>b then [] else a::(range (a+1) b);;

let default_deck = fun () -> 
  List.map (fun x -> Card(x,Spades)) (range 1 10) @
    List.map (fun x -> Card(x,Hearts)) (range 1 10) @
      List.map (fun x -> Card(x,Diamonds)) (range 1 10) @
        List.map (fun x -> Card(x,Clubs)) (range 1 10)
;;

let rec extract i = function
    [] -> failwith "index out of bounds"
  | x::l -> if i=0 then (x,l)
             else let (y,l') = extract (i-1) l
                  in (y, x::l')
;;

let gen_deck = fun () ->
  let rec gen_deck_rec newd remd =
    if remd=[] then newd
    else let i = Random.int (List.length remd)
         in let (c,remd') = extract i remd
            in gen_deck_rec (c::newd) remd'
  in gen_deck_rec [] (default_deck())
;;

is_deck (gen_deck());;
