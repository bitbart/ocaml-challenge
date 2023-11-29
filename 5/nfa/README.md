# Non-deterministic Finite Automata

Consider the following type to represent Finite State Automata (FSAs),
as done in [this exercise](../../4/dfa):
```ocaml
type ('a,'b) fsa = {
  trans: ('a * 'b * 'a) list;      (* set of transitions *)
  init: 'a;                        (* initial state *)
  final: 'a list                   (* final states *)
}
```

In the asserts below, we use the following NFA as example:
```ocaml
let m2 = { 
  trans = [(0,'0',0);(0,'0',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2)];
  init = 0;
  final = [1] }
;;

The project requires to implement the following functions.

## Fat transition relation

`fat_step1 ql a m` computes the state reachable by reading `a` from state `ql`, according to the fat transition relation:
```ocaml
fat_step1 : 'a list -> 'b -> ('a, 'b) fsa -> 'a lis

assert (equals (fat_step1 [0] '0' m2) [1;0]);;
assert (equals (fat_step1 [0] '1' m2) []);;
assert (equals (fat_step1 [0;1] '0' m2) [0;1;2]);;
assert (equals (fat_step1 [0;1] '1' m2) [2]);;
assert (equals (fat_step1 [2] '0' m2) [2]);;
assert (equals (fat_step1 [2] '1' m2) []);;
```

## Subset construction

`subset_construction m` transforms the complete NFA `m` into a complete DFA. 
```ocaml
subset_construction : ('a, 'b) fsa -> ('a list, 'b) fsa

let m2' = subset_construction m2;;

assert(accept ['0'] m2');;
assert(accept ['0';'0'] m2');;
assert(accept ['0';'1';'0'] m2' = false);;
```
