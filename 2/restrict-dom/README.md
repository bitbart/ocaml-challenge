# Restrict domain

Write a function `restrict` that takes as input a function `f` and a predicate `p`, and returns a function `g` that behaves as `f` on the input values that satisfy `p`, and is undefined otherwise.

The function must have the following type:
```ocaml
restrict : ('a -> 'b) -> ('a -> bool) -> 'a -> 'b option
```

Here are some unit tests:
```ocaml
let f1 = restrict succ (fun x -> x>0);;
assert (f1 1 = Some 2);;
assert (f1 0 = None);;

let f2 = restrict (fun (x,y) -> x - y) (fun (x,y) -> x-y>=0);;
assert(f2 (5,2) = Some 3);;
assert(f2 (5,6) = None);;
```