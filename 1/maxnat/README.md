# Max of two natural numbers

Write a function with the following type:
```ocaml
max_nat : int -> int -> int
```
such that ``max_nat a b`` evaluates to the maximum between ``a`` and ``b``, if they are both natural numbers. If not, the function gives an error.

For instance, if the first argument is negative, we will obtain something like:
```ocaml
max_nat (-3) 5 ;;
Exception: "Assert_failure //toplevel//:2:2"
```

**Warning**: do not even think of using the built-in ``max`` function.
