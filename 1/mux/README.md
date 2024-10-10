# Multiplexer

Write a function of type:
```ocaml
mux2 : bool -> bool -> bool -> bool
```
such that 
`mux2 s0 a b` equals to `a` if `s0` is true, otherwise it equals to `b`.

Try with different implementation styles, using:
- the built-in logical connectives `not`, `&&`, `||`;
- conditional expressions;
- pattern matching.

Then, write a function of type:
```ocaml
mux4 : bool -> bool -> bool -> bool -> bool -> bool -> bool
```
such that
`mux4 s1 s0 a0 a1 a2 a3` equals to `ai` if `s1 s0` is the binary encoding of `i`.

The function `mux4` must pass the following unit tests:
```ocaml
assert(mux4 false false false true false true = false);;
assert(mux4 false true false true false true = true);;
assert(mux4 true false false true false true = false);;
assert(mux4 true true false true false true = true);;
```

In the implementation of `mux4`, try to reuse `mux2`.