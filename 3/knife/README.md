# Knife 🔪

Write a function `knife` that cuts a list in two sublists at a given position.
```ocaml
knife : 'a list -> int -> 'a list * 'a list.
```
The element under the knife's position must be included in the left sublist. If the knife is positioned out of bounds, the list stays intact in the output pair.

Here are some unit tests:
```ocaml
assert (knife [1;2;3;4;5;6] 3 = ([1;2;3], [4;5;6]));;
assert (knife ['b';'r';'e';'a';'d'] 3  = (['b';'r';'e'], ['a';'d']));;
assert (knife [] 0 = ([], []));;
assert (knife ["miss"; "me"] 2  = (["miss"; "me"], []));;
assert (knife ["oops"] (-1)  = ([], ["oops"]));;
```