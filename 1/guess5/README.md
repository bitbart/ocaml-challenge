# Guess a number

Write a function with type:
```ocaml
guess5 : int -> bool * int
```
with the following behaviour.
When `guess5` is applied to a parameter n, it first checks if n is in the range 1..5;
if not, it fails.
Otherwise, it generates a random number r between 1 and 5, and it returns a pair (b,r),
where b is true iff n equals to r.

**Hint**. Use the function:
```ocaml
Random.int(n)
```
to generate a random integer in the range 0..n-1.
