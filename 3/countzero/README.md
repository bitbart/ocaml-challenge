# Count zeros of a function

Define a function with type:
```ocaml
countzero : (int -> int) -> int -> int -> int
```
Given as input a function f from integers to integers and
two integers a and `b`,
`countzero f a b` counts the zeros of f in the interval [a,b],
i.e. the number of integer values x in the interval such that f(x)=0. 

For instance, the following `assert` must pass:
```ocaml
assert (countzero (fun x -> x) (-10) 10 = 1);;

assert (countzero (fun x -> x) 1 10 = 0);;

assert (countzero (fun x -> x*x - 1) (-10) 10 = 2);;

assert (countzero (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);;
```