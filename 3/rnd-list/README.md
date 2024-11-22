# Random list

The function `Random.int` in the Ocaml library generates a
random number bounded by a given integer:
```ocaml
# Random.int;;
- : int -> int = <fun>
```

Use this random generator to write a function with type:
```ocaml
val rnd_list : int -> int -> int list = <fun>
```
such that `rnd_list n b` generates a list of `n` elements in the range [1,`b`].
