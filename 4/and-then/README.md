# And then...

Before attempting this exercise, do the exercise [first-third-fifth](../../3/first-third-fifth/README.md).

You are given a new infix operator called "and then" defined as follows:

```ocaml
let ( -?-> ) (o : 'a option) (next : 'a -> 'b option) : 'b option =
  match o with
  | None -> None
  | Some x -> next x
```

It takes an optional value: if it is `None` then it propagates `None` in the return value, otherwise it applies the argument `next` to the value within `Some` to return a new optional value.

It is useful for chaining operations that might fail (where failure is intended as returning `None`) at any point. For example, the following assertions hold:

```ocaml
assert ((
  Some 1 -?-> fun x ->
  Some 2 -?-> fun y ->
  Some 3 -?-> fun z ->
  Some (x + y + z)) = Some 6);;

assert ((
  Some 1 -?-> fun x ->
  None   -?-> fun y ->
  Some 3 -?-> fun z ->
  Some (x + y + z)) = None);;
```

Recall the type of [`first_third_fifth`](../../3/first-third-fifth/README.md), which extracts the first, third and fifth elements from a list, if they exist:
```ocaml
first_third_fifth : 'a list -> ('a * 'a * 'a) option 
```
Redefine it in terms of the `-?->` operator. Does it make the new definition more concise than the old one?
