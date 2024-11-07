# Characteristic functions

Given a set A of elements in a domain U, its **characteristic function** $\chi_A$ is defined as follows:
- $\chi_A(x) = 1$ if $x \in A$
- $\chi_A(x) = 0$ if $x \not\in A$

Implement the characteristic function of the following sets, where the universe is the set of natural numbers. 

For each item i in the list, the functions must have type:
```ocaml
fi : int -> int
```
The functions must be undefined on the negative integers: you can use the `failwith` construct for this purpose.

1. ∅
1. {0, 1, 2}
1. {x | 0 < x < 100}
1. {x | false }
1. {x | ∀y. x + y ≥ x}
1. {x | ∃y. x = y + y}
1. {x | ∃y. y = x + x}
1. {x | ∃y. x < y}
1. {x | ∃y. x * x = y}
1. {x | ∃y. y < 3 ∧ 7 < x y < 20 }
1. {x | x < 50 ∧ ∃y. x = y + y}
1. {z | ∃x. z = 2 x ∧ 0 < 2x < 50 }
1. {z | ∃x, y. z = x + y ∧ x * y ≤ 20}
1. {x | ∀y. y < x → y < 2 }
1. {x | ∀y. (x mod y = 0) → (y=1 ∨ y=x) }
