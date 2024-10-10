# Weekly lectures

OCaml has a convenient syntax to define enumerated types: the elements of the enumeration are listed, separated by `|`.
For example:
```ocaml
type weekday = Mo | Tu | We | Th | Fr

type course = ALF | LIP
```
The elements of the enumeration are always capitalized.

Write a function with type:
```ocaml
isLecture : weekday -> course -> bool
```
such that `isLecture d c` is true when there is a class of course `c`
in the day `d`.

The weekly calendar of lectures is the following:
- ALF: Tuesday, Thursday, Friday
- LIP: Wednesday, Thursday 
