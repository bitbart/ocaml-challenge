# From list to string

Recall the ^ operator to concatenate strings:
```ocaml
# "ping" ^ "pong";;
- : string = "pingpong"
```

Write a function with the following type:
```ocaml
string_of_list : int list -> string
```
such that `string_of_list l` evaluates to a string representation of
the list of integers l.

For instance, we must obtain:
```ocaml
# string_of_list [];;
- : string = "[]"

# string_of_list [1];;
- : string = "[1]"

# string_of_list [1;2;3];;
- : string = "[1;2;3]"
```
