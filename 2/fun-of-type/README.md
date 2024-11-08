# From type to function

Write functions with the following types. For clarity, in the types below we have added parentheses. Recall however that the `->` operator associates to the right, and that `*` has precedence over `->`.

```ocaml
f1 : int -> bool
```

```ocaml
f2 : bool -> int
```

```ocaml
f3 : int -> (int * bool)
```

```ocaml
f4 : (int * bool) -> int
```

```ocaml
f5 : int -> (int -> int)
```

```ocaml
f6 : int -> (int -> bool)
```

```ocaml
f7 : bool -> (int -> bool)
```

```ocaml
f8 : bool -> (bool -> int)
```

```ocaml
f9 : bool -> (int -> int)
```

```ocaml
f10 : (int -> int) -> int
```

```ocaml
f11 : (int -> bool) -> int
```

```ocaml
f12 : (bool -> int) -> int
```

```ocaml
f13 : (int -> bool) -> bool
```

```ocaml
f14 : (bool -> bool) -> int
```

```ocaml
f15 : int -> (int * int) -> int
```

```ocaml
f16 : int -> (int -> (int -> int))
```

```ocaml
f17 : (int -> int) -> (int -> int)
```

```ocaml
f18 : ((int -> int) -> int) -> int
```

```ocaml
f19 : (int -> int) -> (bool -> bool)
```

```ocaml
f20 : (int -> bool) -> (bool -> int)
```

