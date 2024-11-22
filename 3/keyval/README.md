# Key-value maps

Suppose we represent a key-value map as a list of (key,value) pairs.
For example, here is a key-value map where the keys are student names, and the values are their grades in ALF:
```ocaml
[("aru",27);("boi",3);("cao",15);("dui",12);("esu",18)];;
``` 
Using list combinators (no recursion!), write a function:
```ocaml
apply : ('a * 'b) list -> 'a -> 'b option
```
such that `apply f k` returns the value associated to `k`, if any. 
Use the `option` type to handle the possibility that `k` is not associated to any value.
If the list has multiple pairs with the same key, then consider the leftmost one.

Here are some unit tests:
```ocaml
let f0 = [(1, 7); (2, 3); (4, 5); (5, 6); (7, 9); (2, 4); (8, 3)]
assert(apply f0 4 = Some 5);;
assert(apply f0 6 = None);;
assert(apply f0 2 = Some 3);;
```

Always using list combinators (no recursion!), write a function:
```ocaml
mkfun : ('a * 'b) list -> ('a * 'b) list
```
such that `mkfun f` removes from `f` the key-value pairs with duplicate keys.
Namely, if `f` contains a pair `(k,v)` and another pair `(k,v')`, then `mkfun` removes the rightmost pair in the list.
The ordering of the other pairs must be preserved.

Here are some unit tests:
```ocaml
assert(mkfun [(1,7);(2,3)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(1,3)] = [(1,7)]);;
assert(mkfun [(1,7);(2,3);(1,5)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(2,3);(1,5);(1,8)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(2,3);(1,5);(1,8);(2,4)] = [(1,7);(2,3)]);;
```
