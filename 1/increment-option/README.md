# Increment optional value

In OCaml you can use values of type `option` via pattern matching. 
For example, the following function takes as input a value of type `int option` and returns 0 or the value within the Some tag:
```ocaml
let f x = match x with
  None -> 0
| Some x -> x;;

f (Some 3);;
f None;;
```

Write a function of type:
```ocaml
incr_opt : int option -> int option
```
that takes an int option and returns `Some(value + 1)` if the input is `Some(value)`. If the input is `None`, return `None`.

**Examples**
```ocaml
incr_opt (Some 5) = Some 6
incr_opt None = None
```
