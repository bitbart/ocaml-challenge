# Option map

Write a function `option_map` that takes a function `f` and an `option` value, and returns: 
- `None`, if the input value is `None`;
- `Some (f x)`, if the input value is `Some x`.

The function must have the following type:
```ocaml
option_map : ('a -> 'b) -> 'a option -> 'b option
```

Here are some unit tests:
```ocaml
let double x = x * 2;;
let square x = x * x;;

assert (option_map double (Some 3) = Some 6);;
assert (option_map double None = None);;
assert (option_map double (Some (square 3)) = Some 18);;
assert (option_map square (option_map double None) = None);;
```
