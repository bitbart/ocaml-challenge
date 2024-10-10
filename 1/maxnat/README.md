# Max of two natural numbers

Write a function with the following type:
```ocaml
max_nat : int -> int -> int
```
such that ``max_nat a b`` evaluates to the maximum between ``a`` and ``b``,
if they are both *natural* numbers (i.e., greater than or equal to 0).
If not, the function must fail.

To make a function fail, use the `failwith` construct. For example:
```ocaml
let always_fail x = failwith("This function always fails")
```
The following tests must pass:
```ocaml
assert(max_nat 2 5 = 5);;
assert(max_nat 5 2 = 5);;
assert(try max_nat (-2) 5 |> fun _ -> false with _ -> true);;
assert(try max_nat 2 (-5) |> fun _ -> false with _ -> true);;
assert(try max_nat (-2) (-5) |> fun _ -> false with _ -> true);;
```

**Warning**: do not even think of using the built-in ``max`` function.
