# Perforated list

Write a function with type:
```ocaml
is_perforated : int list -> bool
```
that checks if a list of integers is **perforated**. 
A list is said to be perforated if, for every pair of consecutive elements in the list, there exists at least one integer strictly between them.

For example, the list `[1;3;7]` is perforated, because: 
- between the consecutive list elements 1 and 3 there exists some integer (e.g., 2); 
- between the consecutive list elements 3 and 7 there exists some integer (e.g., 5).

Instead, the list `[1;3;2]` is not perforated, because:
- between the consecutive list elements 3 and 2 there exists no integer. 

Here are some additional unit tests:
```ocaml
assert(is_perforated []);;
assert(is_perforated [1]);;
assert(is_perforated [1;2] = false);;
assert(is_perforated [1;3]);;
assert(is_perforated [1;5;2]);;
assert(is_perforated [1;3;2] = false);;
assert(is_perforated [1;4;2;0]);;
assert(is_perforated [1;3;2;0] = false);;
assert(is_perforated [1;3;5;2;4;7;3;1]);;
```