# Rotate list

Write a function with type:
```ocaml
rotate : int -> 'a list -> 'a list
```
such that `rotate n l` rotates the list `l` by `n` positions to the left.
For instance, rotating the list `[1;2;3]` by 1 position yields `[2;3;1]`.

Other examples are shown by the following asserts:
```ocaml
assert(rotate 0 [5;6;7;8] = [5;6;7;8]);;  
assert(rotate 1 [5;6;7;8] = [6;7;8;5]);;  
assert(rotate 2 [5;6;7;8] = [7;8;5;6]);;  
assert(rotate 3 [5;6;7;8] = [8;5;6;7]);;  
assert(rotate 4 [5;6;7;8] = [5;6;7;8]);;  
```
