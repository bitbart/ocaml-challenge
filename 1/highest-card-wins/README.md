# Highest card wins

Consider a simple card game between a player and the dealer, with the following rules:
- if neither the player nor the dealer have the Joker, then the one with the highest card wins;
- if both the player and the dealer have the Joker, then the dealer wins;
- otherwise, the one with the Joker wins.

We represent a card with the following type:
```ocaml
type card = Joker | Val of int;;
```
where the value of the card is assumed to be in the range [1,10].

Write a function with the following type:
```ocaml
win : card -> card -> bool
```
which returns true iff the player wins (we assume that the card of the player is the first parameter of ``win``).
