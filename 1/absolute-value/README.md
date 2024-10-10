# Absolute value

Implement a function with the following type:
```ocaml
f : int -> int
```
such that f x is the absolute value of x.

Before starting the exercise, experiment with utop or [TryOCaml](try.ocamlpro.com) to learn a few arithmetic operators in OCaml:
```ocaml
(* integer subtraction *)
2 - 5;;

(* integer opposite *)
-3;;

(* mixing integer subtraction with opposite *)
2 - (-5);;

(* testing the less-than relation *)
2 < 5;;
5 < 2;;
```
Recall that you can use any expression of type `bool` as a guard in a conditional expression:
```ocaml
if 2 < 5 then 2 else 5;;
if 5 < 2 then 2 else 5;;
```
