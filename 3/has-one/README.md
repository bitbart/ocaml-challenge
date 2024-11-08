# Has One

Write a function with type:
```ocaml
has_one : int -> bool
```
For a positive integer n, `has_one n` returns true if n contains a 1 digit.
If n is negative, then the function must fail.

Here are some unit tests:
```ocaml
assert(has_one 10 = true);;
assert(has_one 220 = false);;
assert(has_one 911 = true);;
assert(has_one 451 = true);;
assert(try has_one (-1) |> fun _ -> false with _ -> true);;
```