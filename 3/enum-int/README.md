# Enumeration of integers

Write a function with type
```ocaml
enum_int : int -> int
```
which maps each natural number to an integer,
producing the following enumeration:

0   -1   1   -2   2   -3   3   ...

Your function must pass the test:
```ocaml
assert (List.init 10 enum_int = [0; -1; 1; -2; 2; -3; 3; -4; 4; -5]);;
```