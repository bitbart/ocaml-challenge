# Merge Sort

Let's implement the merge sort algorithm for integer lists in OCaml.

First, write the function `merge` that takes two
sorted lists and threads their elements together to form a new sorted list:
```ocaml
merge : int list -> int list -> int list
```

Next, define a function `halve` that splits a list in half and returns a pair with the two halves:
```ocaml
halve : int list -> int list * int list
```
If the length of list is odd, `halve` puts the central element in the right half.

*Tip*: use the [knife](../knife/README.md) function from an earlier exercise.

Finally, use `merge` and `halve` to define `merge_sort`:
```ocaml
merge_sort : int list -> int list
```

Here are some unit tests:

```ocaml
assert (merge [1;4;5] [2;3;6] = [1;2;3;4;5;6]);;
assert (merge [7] [2;3;6] = [2;3;6;7]);;
assert (merge [7] [] = [7]);;

assert (halve [1;3;5;8;-2;6] = ([1;3;5], [8;-2;6]));;
assert (halve [1;3] = ([1], [3]));;
assert (halve [1;3;5] = ([1], [3;5]));;

assert (merge_sort [1;3;5;8;-2;6] = [-2;1;3;5;6;8]);;
```