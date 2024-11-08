# Optional application

Define an operator with type:

```ocaml
( <*> ) : ('a -> 'b) option -> 'a option -> 'b option
```

It accepts an optional function and maps it to an optional argument. If there is no function, the result is `None`, otherwise the result is the same as [`option_map`](/2/option-map/README.md).

It must pass these unit tests:

```ocaml
let square x = x * x
let double x = 2 * x
let multiply x y = x * y;;

assert (Some square <*> None = None);;
assert (None <*> Some 2 = None);;
assert (None <*> (double <*> Some 2) = None);;
assert (Some multiply <*> Some 3 <*> Some 2 = Some 6);;
assert (Some multiply <*> None <*> Some 2 = None);;
``` 