# Consensus3

Define a function with type:
```ocaml
consensus3 : ('a -> 'b) * ('a -> 'b) * ('a -> 'b) -> 'a -> 'b option
```
The function takes as input 3 functions with the same type,
and it outputs a function with type:
```ocaml
'a -> 'b
```
By applying `consensus3` to a triple of functions `(f0,f1,f2)`,
the output function `f` satisfies the following properties:
- `f n` is undefined if at least one of the 3 input functions is undefined on the input `n`
- `f n = Some v` if at least 2 out of 3 input functions give `v` as output on the input `n`
- `f n = None` otherwise.

For instance:
```ocaml
assert (try
  consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 0 = None
  with _ -> true);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 1 = Some 5);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 2 = Some 2);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 3 = None);;
```