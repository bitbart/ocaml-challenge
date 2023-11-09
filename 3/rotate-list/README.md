# Rotate list

Define a function with type:
```ocaml
rotate : int -> 'a list -> 'a list
```
such that `rotate n l` rotates `l` leftwards by `n` positions.

For instance, the function must pass the following assertions:
```ocaml
assert(rotate 0 [5;6;7;8] = [5;6;7;8]);;  
assert(rotate 1 [5;6;7;8] = [6;7;8;5]);;  
assert(rotate 2 [5;6;7;8] = [7;8;5;6]);;  
assert(rotate 3 [5;6;7;8] = [8;5;6;7]);;  
assert(rotate 4 [5;6;7;8] = [5;6;7;8]);;
```