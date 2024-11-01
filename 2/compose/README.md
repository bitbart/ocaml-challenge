# Function composition

Write a function `comp` that takes as input two functions f and g and returns a new function that is the composition of f and g (i.e., f (g x)).

The function must have the following type:
```ocaml
comp : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
```

Here are some unit tests:
```ocaml
let double x = x * 2;;
let square x = x * x;;

assert((comp square double) 3 = 36);;
assert((comp double square) 3 = 18);;
assert((comp String.uppercase_ascii String.trim) "hello" = "HELLO");;
```