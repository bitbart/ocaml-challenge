# Consecutive even

Define a function with type:
```ocaml
consecutive_even : int list -> int
```
such that `consecutive_even l` gives the length of the longest subsequence of `l` containing only even numbers.

For instance, the function must pass the following asserts:
```ocaml
assert(consecutive_even [] = 0);;
assert(consecutive_even [1;2;3;4;5;6] = 1);; 
assert(consecutive_even [1;2;2;3;4;5] = 2);;
assert(consecutive_even [1;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;2;3;4;2;6;5] = 3);;
```
