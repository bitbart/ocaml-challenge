# Partial functions

Assuming to represent finite sets as lists,
write the following functions:
```ocaml
is_fun : 'a list -> 'b list -> ('a * 'b) list -> bool
```
`is_fun _A _B f` returns true iff `f` represents a partial function from `_A` to `_B`.

```ocaml
is_tot : 'a list -> 'b list -> ('a * 'b) list -> bool
```
`is_tot _A _B f` returns true iff `f` is a total function from `_A` to `_B`.

```ocaml
is_inj : 'a list -> 'b list -> ('a * 'b) list -> bool
```
assuming that `f` is a partial function from `_A` to `_B`,
`is_inj _A _B f` returns true iff `f` is injective.

```ocaml
is_surj : 'a list -> 'b list -> ('a * 'b) list -> bool
```
assuming that `f` is a partial function from `_A` to `_B`,
`is_surj _A _B f` returns true iff `f` is surjective.

```ocaml
rel_of_fun : 'a list -> ('a -> 'b) -> ('a * 'b) list
```
assuming that `f` is an OCaml function from `_A` (to some set),
`rel_of_fun _A f` is the graph of the function, i.e. the list of pairs (x,f x).

```ocaml
bot : 'a -> 'b
```
is the OCaml function which has empty domain.

```ocaml
bind : ('a -> 'b) -> 'a -> 'b -> 'a -> 'b
```
`bind f a b` is the OCaml function which maps a to b, and is equal to f
in every other element.

```ocaml
fun_of_rel : ('a * 'b) list -> ('a -> 'b) *) 
```
`fun_of_rel f` converts the partial function f (represented as a list of pairs)
into an OCaml function.
   
```ocaml
is_mono : ('a * 'b) list -> bool
```
assuming that `f` is a partial function, `is_mono f` returns true iff it is monotonic.
