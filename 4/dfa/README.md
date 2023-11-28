# Deterministic Finite Automata

Consider the following type to represent Finite State Automata:
```ocaml
type 'a fsa = {
  trans: (int * 'a * int) list;     (* set of transitions *)
  init: int;                        (* initial state *)
  final: int list                   (* final states *)
}
```
where the sets of states and labels can be inferred by the transition relation.
We assume that set are represented as lists, as done in [this exercise](../3/sets).

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
We also use the following functions to compare sets:
```ocaml
let subseteq l l' = List.fold_right (fun x y -> if List.mem x l' then y else false) l true;;

let equals l l' = subseteq l l' && subseteq l' l;;
```

The project requires to implement the followinfg functions.


## Get labels

`getlabels m` outputs the set of labels in the FSA `m`.

```ocaml
getlabels : 'a fsa -> 'a list

assert (equals (getlabels m1) ['0';'1']);;
assert (equals (getlabels m2) ['0';'1']);;
assert (equals (getlabels m3) ['0';'1']);;
```

## Get outgoing labels

`outlabels m q` outputs the set of outgoing labels from the state `q`.

```ocaml
outlabels : 'a fsa -> int -> 'a list

assert (equals (outlabels m1 0) ['0';'1']);;
assert (equals (outlabels m1 1) ['0';'1']);;
assert (equals (outlabels m1 2) ['0';'1']);;
assert (equals (outlabels m2 2) ['0']);;
```

## Get states

`getstates m` outputs the set of states of the FSA `m`.
```ocaml
getstates : 'a fsa -> int list

assert (equals (getstates m1) [0;1;2]);;
assert (equals (getstates m2) [0;1;2]);;
assert (equals (getstates m3) [0;1;2]);;
```

## Check is FSA is complete

`is_complete m` evaluates to true iff the FSA `m` is complete,
that is for each state there are outgoing transitions for all labels.
```ocaml
is_complete : 'a fsa -> bool

assert (is_complete m1);;
assert (is_complete m2 = false);;
assert (is_complete m3 = false);;
```

## Check is FSA is deterministic 

`is_deterministic m` evaluates to true iff the FSA `m` is deterministic.
```ocaml
is_deterministic : 'a fsa -> bool

assert (is_deterministic m1);;
assert (is_deterministic m2 = false);;
assert (is_deterministic m3);;
```

## Single transition step

`step1 q a m` takes as input a state `q`, a label `a` and a complete DFA `m`, and outputs the state reached by reading `a` in `q`.
```ocaml
step1 : int -> 'a -> 'a fsa -> int

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
step : int -> 'a list -> 'a fsa -> int

assert(step 0 ['0';'0';'0'] m1 = 0);;
assert(step 0 ['0';'1';'1'] m1 = 2);;
```

## Check if a word is accepted

`accept w m` evaluates to true iff the word `w` is accepted by the complete DFA `m`.
```ocaml
accept : 'a list -> 'a fsa -> bool

assert (accept ['0';'0';'1'] m1);;
assert (accept ['0';'0';'1';'1'] m1 = false);;
assert (accept ['1';'0';'0';'1'] m1 = false);;
```

## Completing a DFA

`complete m q` constructs a FSA that completes the DFA `m` with the sink state `q`.
```ocaml
complete : 'a fsa -> int -> 'a fsa

let m3' = complete m3 3;;
assert (accept ['0';'1';'0';'1'] m3');;
assert (accept ['0';'0';'1';'0';'0'] m3');;
assert (accept ['0';'1';'1';'0'] m3' = false);;
```
