# Sum range

Define a function with type:
```ocaml
sumrange : int -> int -> int
```
such that `sumrange a b` sums the numbers in the range `[a,b]`.
If the range is empty, then the result is zero.

Here are some unit tests:
```ocaml
assert (sumrange 0 1 = 1);;
assert (sumrange 1 3 = 6);;
assert (sumrange 3 2 = 0);;
```