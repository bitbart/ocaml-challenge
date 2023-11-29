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
which computes the average of a list of valid grades (it fails if some grade in the list is not valid).
