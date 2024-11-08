# Alternative

Define a function with type:

```ocaml
( <|> ) : 'a option -> 'a option -> 'a option
```

That returns the first value available within the `Some` tag or `None` if both arguments are `None`.

Here are some unit tests:

```ocaml
assert (Some true <|> Some false = Some true);;
assert (None <|> Some false = Some false);;
assert (Some 3 <|> None = Some 3);;
assert (Some "cat" <|> Some "dog" = Some "cat");;
assert (None <|> Some "dog" = Some "dog");;
```
