# Deck of cards

Consider the following types to represent French-suited cards:
```ocaml
type suit = Spades | Hearts | Diamonds | Clubs;;
type card = Card of int * suit;;
```
A card is valid if its first element is a value comprised between 1 (Ace) and 10 (King).

A **deck** is a list of valid cards without duplicates, and it is
complete if it includes exactly 40 cards.

Write a function with type:
```ocaml
is_complete : card list -> bool
```
which evaluates to true iff the argument is a complete deck.

Then, recall the Ocaml function to generate random numbers bounded by a given integer:
```ocaml
# Random.int;;
- : int -> int = <fun>
```

Use this random generator to write a function with type:
```ocaml
gen_deck : unit -> card list
```
which generates a complete random deck of cards.