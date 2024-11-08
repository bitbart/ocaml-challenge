# Function composition

Write a function `comp` that takes as input two functions f and g and returns a new function h that is the composition of f and g, that is: 
```
h(x) = f (g x)
```
The function must have the following type:
```ocaml
comp : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
```
where `f : 'b -> c'`, `g : 'a -> 'b`, and the type of the result is `'a -> 'c`.

Here are some unit tests:
```ocaml
let double x = x * 2;;
let square x = x * x;;

assert((comp square double) 3 = 36);;
assert((comp double square) 3 = 18);;

assert(comp (fun b -> if b then 0 else 1) (fun x -> x>0) 3 = 0);;
assert(comp (fun b -> if b then 0 else 1) (fun x -> x>0) 0 = 1);;
```
