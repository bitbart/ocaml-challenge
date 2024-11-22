# Min and max of a function

Recall the built-in option type:
```ocaml
type 'a option = Some of 'a | None
```

Write a function with the following type:
```ocaml
minmaxfun : (int -> 'a) -> int -> int -> ('a * 'a) option
```
such that ``minmaxfun f a b`` computes the minimum and the maximum
of f in the range [a,b].
If the range is empty, then the function evaluates to ``None``.

Your function must pass the following unit tests:
```ocaml
assert (minmaxfun (fun x -> x) (-2) 5 = Some (-2,5));;
assert (minmaxfun (fun x -> x) 5 (-2) = None);;
assert (minmaxfun (fun x -> x) 5 5 = Some (5,5));;
assert (minmaxfun (fun x -> x * x) (-2) 5 = Some (0,25));;

let curve x = x |> Float.of_int |> fun x -> x ** 3.0 -. 3.0 *. x;;
let arccos x = x |> Float.of_int |> Float.acos;;

assert (minmaxfun curve (-2) 2 = Some (-2.0,2.0));;
assert (minmaxfun arccos (-1) 1 = Some (0., Float.pi));;
```