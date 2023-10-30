# Weekly lectures

Consider the following types:
```ocaml
type weekday = Mo | Tu | We | Th | Fr

type course = ALF | LIP
```

Write a function with type:
```ocaml
isLecture : weekday -> course -> bool
```
such that `isLecture d c` is true when there is a class of course `c`
in the day `d`.

The weekly calendar of lectures is the following:
- ALF: Tuesday, Thursday, Friday
- LIP: Wednesday, Thursday 
