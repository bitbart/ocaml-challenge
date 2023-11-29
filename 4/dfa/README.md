# Deterministic Finite Automata

Consider the following type to represent Finite State Automata (FSAs):
```ocaml
type ('a,'b) fsa = {
  trans: ('a * 'b * 'a) list;      (* set of transitions *)
  init: 'a;                        (* initial state *)
  final: 'a list                   (* final states *)
}
```
The sets of states and labels of the FSA can be inferred by the transition relation.
We assume that set are represented as lists, as done in [this exercise](../../3/set).

In the asserts below, we use the following FSAs as examples:
```ocaml
(* m1 deterministic and complete *)
let m1 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2);(2,'1',2)];
  init = 0;
  final = [1] }
;;

(* m2 non-deterministic and non-complete *)
let m2 = { 
  trans = [(0,'0',0);(0,'0',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2)];
  init = 0;
  final = [1] }
;;

(* m3 deterministic and non-complete *)
let m3 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',1);(1,'1',2)];
  init = 0;
  final = [1;2] }
;;
```
In the asserts, we will use the following functions to compare sets:
```ocaml
let rec subseteq xl yl = match xl with
    [] -> true
  | x::xl' -> mem x yl && subseteq xl' yl;;

let seteq xl yl = (subseteq xl yl) && (subseteq yl xl);;
```

The project requires to implement the following functions on FSAs.


## Get labels

`getlabels m` outputs the set of labels in the FSA `m`.

```ocaml
getlabels : ('a,'b) fsa -> 'b list

assert (seteq (getlabels m1) ['0';'1']);;
assert (seteq (getlabels m2) ['0';'1']);;
assert (seteq (getlabels m3) ['0';'1']);;
```

## Get outgoing labels

`outlabels m q` outputs the set of outgoing labels from the state `q`.

```ocaml
outlabels : ('a,'b) fsa -> 'a -> 'b list

assert (seteq (outlabels m1 0) ['0';'1']);;
assert (seteq (outlabels m1 1) ['0';'1']);;
assert (seteq (outlabels m1 2) ['0';'1']);;
assert (seteq (outlabels m2 2) ['0']);;
```

## Get states

`getstates m` outputs the set of states of the FSA `m`.
```ocaml
getstates : ('a,'b) fsa -> 'a list

assert (seteq (getstates m1) [0;1;2]);;
assert (seteq (getstates m2) [0;1;2]);;
assert (seteq (getstates m3) [0;1;2]);;
```

## Check if FSA is complete

`is_complete m` evaluates to true iff the FSA `m` is complete,
that is for each state there are outgoing transitions for all labels.
```ocaml
is_complete : ('a,'b) fsa -> bool

assert (is_complete m1);;
assert (is_complete m2 = false);;
assert (is_complete m3 = false);;
```

## Check if FSA is deterministic 

`is_deterministic m` evaluates to true iff the FSA `m` is deterministic.
```ocaml
is_deterministic : ('a,'b) fsa -> bool

assert (is_deterministic m1);;
assert (is_deterministic m2 = false);;
assert (is_deterministic m3);;
```

## Single transition step

`step1 q a m` takes as input a state `q`, a label `a` and a complete DFA `m`, and outputs the state reached by reading `a` in `q`.
```ocaml
step1 : 'a -> 'b -> ('a,'b') fsa -> 'a

assert (step1 0 '0' m1 = 0);;
assert (step1 0 '1' m1 = 1);;
assert (step1 1 '0' m1 = 2);;
assert (step1 1 '1' m1 = 2);;
assert (step1 2 '0' m1 = 2);;
assert (step1 2 '1' m1 = 2);;
```

## Sequence of transition steps

`step q w m` takes as input a state `q`, a word `w` and a complete DFA `m`, and outputs the state reached by reading `w` in `q`.
```ocaml
step : 'a -> 'b list -> ('a,'b) fsa -> 'a

assert(step 0 ['0';'0';'0'] m1 = 0);;
assert(step 0 ['0';'1';'1'] m1 = 2);;
```

## Check if a word is accepted

`accept w m` evaluates to true iff the word `w` is accepted by the complete DFA `m`.
```ocaml
accept : 'a list -> ('b,'a) fsa -> bool

assert (accept ['0';'0';'1'] m1);;
assert (accept ['0';'0';'1';'1'] m1 = false);;
assert (accept ['1';'0';'0';'1'] m1 = false);;
```

## Completing a FSA

`complete m q` constructs the completion of a FSA `m` with the sink state `q`.
```ocaml
complete : ('a,'b) fsa -> 'a -> ('a,'b) fsa

let m3' = complete m3 3;;
assert (is_complete m3');;
assert (accept ['0';'1';'0';'1'] m3');;
assert (accept ['0';'0';'1';'0';'0'] m3');;
assert (accept ['0';'1';'1';'0'] m3' = false);;
```
