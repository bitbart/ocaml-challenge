# fold_left with index inputs

Define a function with type:

```ocaml
fold_lefti : ('acc -> int -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
```

This function behaves much like `List.fold_left`, except that it exposes the indexes of the list's elements in the combinator argument.

In practice, the first parameter of `fold_lefti` is a function that takes a number in addition to the usual accumulator and element parameters. The argument to this `int` parameter is a number equal to the index of each element that `fold_lefti` encounters as it descends in the list.

Formally, `fold_lefti f init [b1;b2;...;bn]` must evaluate to `f (... (f (f init 1 b1) 2 b2) ...) n bn`.

For instance, `fold_lefti` could be used to sum together the elements of a list at even positions, while ignoring the others:
```ocaml
let fold_left_even f = fold_lefti (fun acc i a -> if i mod 2 = 0 then f acc a else acc);;

assert (fold_left_even ( + ) 0 [1;2;3;4;5] = 9);;
```