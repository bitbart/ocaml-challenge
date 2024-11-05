# And then...

It is strongly recommended that you do [first-third-fifth](../../3/first-third-fifth/README.md) before attempting this exercise.

You are given a new infix operator called "and then" defined as follows:

```ocaml
let ( -?-> ) (o : 'a option) (next : 'a -> 'b option) : 'b option =
  match o with
  | None -> None
  | Some x -> next x
```

It takes an optional value: if it is `None` then it propagates `None` in the return value, otherwise it applies the argument `next` to the value within `Some` to return a new optional value.

It is useful for chaining operations that might fail at any point (where failure is intended as returning `None`). The assertions below show how it is used to create a pipeline for summing three optional integers:

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

A common operation that notoriously fails is index lookup on a list: what to do if the index is out of bounds? Languages like Python throw an exception and make the whole program fail; C lets you access the out-of-bounds location anyway and possibly corrupt your memory. Others like Javascript return a special `undefined` value.

In OCaml index lookup can be performed in a safe manner with the `List.nth_opt` function of the standard library. It uses the `option` type to wrap the result of the lookup:
```
List.nth_opt : 'a list -> int -> 'a option
```
If the list is too short it returns `None`, otherwise it returns the n-th element inside the `Some` constructor.

Recall the exercise [first_third_fifth](../../3/first-third-fifth/README.md), where you had to define a function that extracts the first, third and fifth elements from a list that is long enough. Its type was:
```ocaml
first_third_fifth : 'a list -> ('a * 'a * 'a) option 
```
Your task is to redefine it in terms of the `-?->` operator.

Read each position with `List.nth_opt` and pipe its output through `-?->` to an anonymous function that performs the next read. Repeat until you have enough values to form a triple.

Is the new definition shorter than the old one?
