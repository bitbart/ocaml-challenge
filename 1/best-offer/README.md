# Best offer

You are given three offers, each represented with the type `int option`. An offer is `Some(x)` means that `x` is the amount of the offer, while `None` means that no offer is made.

Write a function of type:
```ocaml
best_offer : int option -> int option -> int option -> int option
```
that takes three offers and returns the highest one. If none of the offers are valid (i.e., all are `None`), the function should return `None`.

**Examples**

```ocaml
best_offer (Some 100) (Some 200) (Some 150) = Some 200

best_offer (Some 100) None (Some 150) = Some 150

best_offer None None None = None

best_offer None (Some 300) None = Some 300
```
