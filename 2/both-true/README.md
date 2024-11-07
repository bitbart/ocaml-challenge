# Both true

Write a function `both_true` that takes as input two predicates `p` and `q`, each of type `'a -> bool`, and returns a new function that checks if both `p` and `q` are true for a given argument.

The function must have the following type:
```ocaml
both_true : ('a -> bool) -> ('a -> bool) -> 'a -> bool
```

Here are some unit tests:
```ocaml
let is_positive x = x > 0;;
let is_even x = x mod 2 = 0;;
let is_positive_and_even = both_true is_positive is_even;;
assert(is_positive_and_even 4 = true);;
assert(is_positive_and_even 3 = false);;
```