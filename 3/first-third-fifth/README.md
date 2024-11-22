# 1st, 3rd and 5th

Write a polymorphic function that returns the first, third and fifth element of a list. Its type must be:

```ocaml
first_third_fifth : 'a list -> ('a * 'a * 'a) option 
```

If the list is not long enough, the result is `None`, otherwise it is a triple of values wrapped in the constructor `Some`.

For example:

```ocaml
first_third_fifth ["cat"; "dog"] = None
first_third_fifth [1; 2; 3; 4; 5; 6] = Some (1, 3, 5)
```