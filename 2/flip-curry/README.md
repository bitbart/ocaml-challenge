# Flip arguments in curried functions

Write a function `flip` that takes as input a function `f` in curried form and returns a new function that works as `f` but takes its arguments in the opposite order. 

The function must have the following type:
```ocaml
flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
```

Here are some unit tests:
```ocaml
let sub x y = x - y;;
let flipped_sub = flip sub;;
assert (flipped_sub 3 10 = 7);;
assert (flipped_sub 10 3 = -7);;
```
