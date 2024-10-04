Define a function with type:

```ocaml
fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b list -> 'a
```

This function behaves like `List.fold_left`, with the exception that the function parameter of `fold_lefti` also takes as input the index matching each element of the list which it iterates through.

Formally, `fold_lefti f init [b1;b2;...;bn]` must evaluate to `f (... (f (f init 1 b1) 2 b2) ...) n bn`.

For instance, `fold_lefti` could be used to iterate on the elements of a list at even positions:
```ocaml
let fold_left_even f = fold_lefti (fun acc i a -> if i mod 2 = 0 then f acc a else acc);;

assert (fold_left_even ( + ) 0 [1;2;3;4;5] = 9);;
```