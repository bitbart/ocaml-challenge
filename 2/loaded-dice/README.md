# Loaded dice

Write a function with type:
```ocaml
dice : int -> int
```
which throws a loaded dice, where the probability of a 6 is specified by the argument (in percentage),
while the other sides are uniform.
For instance, `dice 80` will throw a loaded dice where the probability of 6 is 80%,
while the probability of the other sides is 4%.