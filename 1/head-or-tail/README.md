# Head or tail

Write a function with the following type:
```ocaml
hot : unit -> string
```
such that `hot()` evaluates to the string `"head"` or `"tail"` depending on a bit generated uniformly at random.

**Hint**. Use the function:
```ocaml
Random.int(n)
```
to generate a random integer in the range 0..n-1.
