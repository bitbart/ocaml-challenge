# Student grades

Consider the following type to represent student grades:
```ocaml
type grade = Val of int | CumLaude;;
```
Write functions with the following types:
```ocaml
is_valid : grade -> bool
```
which evaluates to true iff the grade is included between 18 and 30, or it is ``CumLaude``.

```ocaml
int_of_grade : grade -> int
```
which transforms a valid grade into an integer (where ``CumLaude`` counts for 32), and fails if the grade is not valid.

```ocaml
avg : grade list -> float
```
which computes the average of a list of *valid* grades (it fails if some grade in the list is not valid).

Finally, write a function with type: 
```ocaml
avg_norec : grade list -> float
```
that takes as input a list of grades (not necessarily valid) and outputs the average of all the valid grades in the list. This function must use list combinators and function pipelining, and must not use recursion. 
