# Enumeration of pairs of naturals

Write a function with type
```ocaml
enum_nat_nat : int -> int * int
```
which maps each natural number to a pair of naturals,
producing the following enumeration:

(0, 0)
(0, 1)   (1, 0)
(0, 2)   (1, 1)   (2, 0)
(0, 3)   (1, 2)   (2, 1)   (3, 0)
(0, 4)   (1, 3)   (2, 2)   (3, 1)   (4, 0)
(0, 5) ...