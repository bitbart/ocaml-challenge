# Extract an element from a list

Write a function to extract the element at a given index from a list.
The function must have the following type:
```ocaml
extract : int -> 'a list -> 'a * 'a list
```
The semantics of `extract i l` is the following:
- if the index i is within the bounds of the list, then it evaluates to a pair
(x,l'), where x is the element of l at index i, and l' is the list obtained by
removing x from l';
- otherwise, the evaluation throws a failure exception.

For example:
```ocaml
# extract 0 [1;2;3];;
- : int * int list = (1, [2; 3])

# extract 1 [1;2;3];;
- : int * int list = (2, [1; 3])

# extract 2 [1;2;3];;
- : int * int list = (3, [1; 2])

# extract 3 [1;2;3];;
Exception: Failure "index out of bounds".
```