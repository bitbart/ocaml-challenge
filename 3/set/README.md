# Sets

A set can be represented as a list without duplicates, ignoring the ordering of elements.
Write the following functions that operate on sets represented as lists.


## Set membership

`mem x s` is true iff the element `x` belongs to the set `s`.
```ocaml
mem : 'a -> 'a list -> bool

assert(mem 1 [1;3;5]);;
assert(mem 2 [1;3;5] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[1;2]]);;
```

## Check set inclusion

`subseteq xl yl` is true iff `xl` is a subset of `yl`.
```ocaml
subseteq : 'a list -> 'a list -> bool

assert(subseteq [] [1;3;5]);;
assert(subseteq [1;5] [5;1]);;
assert(subseteq [1;5] [1;3;5]);;
assert(subseteq [1;5] [5;3;1]);;
assert(subseteq [2] [1;3;5] = false);;
assert(subseteq [[1;2]] [[1];[2];[2;1]] = false);;
assert(subseteq [[1];[2;1]] [[1];[2];[2;1]]);;
```

## Check set equality

`seteq xl yl` is true iff `xl` is a equal to `yl` up-to reordering of elements.
```ocaml
seteq : 'a list -> 'a list -> bool

assert(seteq [1;5;3] [1;3;5]);;
assert(seteq [1;5;2] [1;3;5] = false);;
assert(seteq [[1;2]] [[2;1]] = false);;
assert(seteq [[1];[1;2]] [[1;2];[1]]);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
```

## Check for duplicates

`dup l` evaluates to true iff the list `l` contains duplicates (and therefore, it is not a set).
```ocaml
dup : 'a list -> bool

assert(dup [] = false);;
assert(dup [1;1]);;
assert(dup [1;3;5] = false);;
assert(dup [1;3;5;3]);;
```

## Construct set from list

`mkset l` removes the duplicates from the list `l`, making is a set.
```ocaml
mkset : 'a list -> 'a list

assert(seteq (mkset [1;2;3;2;1]) [1;2;3]);;
assert(seteq (mkset [1;2;1;2;1]) [1;2]);;
assert(seteq (mkset [1;2;3]) [2;3;1]);;
```

## Set union, intersection, and difference

The following functions compute the union, intersection and difference of two sets.
```ocaml
union : 'a list -> 'a list -> 'a list
inter : 'a list -> 'a list -> 'a list
diff : 'a list -> 'a list -> 'a list

assert(seteq (union [1;2;3] []) [1;2;3]);;
assert(seteq (union [] [2;3;4]) [2;3;4]);;
assert(seteq (union [1;2;3] [2;3;4]) [1;2;3;4]);;

assert(seteq (inter [1;2;3] []) []);;
assert(seteq (inter [] [2;3;4]) []);;
assert(seteq (inter [1;2;3] [2;3;4]) [2;3]);;

assert(seteq (diff [1;2;3] []) [1;2;3]);;
assert(seteq (diff [] [2;3;4]) []);;
assert(seteq (diff [1;2;3] [2;3;4]) [1]);;
assert(seteq (diff [1;2;3] [3;1]) [2]);;
```

## Disjoint union

`dsum xl yl` computes the disjoint union between `xl` and `yl`.
```ocaml
dsum : 'a list -> 'a list -> (int * 'a) list

assert(seteq (dsum [1;2;3] []) [(0,1);(0,2);(0,3)]);;
assert(seteq (dsum [] [2;3;4]) [(1,2);(1,3);(1,4)]);;
assert(seteq (dsum [1;2] [2;3]) [(0,1);(0,2);(1,2);(1,3)]);;
```

## Power set

`powset xl` computes the set of all subsets of `xl`.

```ocaml
powset : 'a list -> 'a list list

assert (powset [] = [[]]);;
assert (seteq (powset [1]) [[];[1]]);;
assert (List.length (powset [1;2]) = 4);;
assert (List.length (powset [1;2;3]) = 8);;
assert (List.length (powset [1;2;3;4]) = 16);;
```