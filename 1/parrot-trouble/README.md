# Parrot trouble

In the apartment next to yours there is an annoying talking parrot.
The regulations of your flat prohibit making noise before 7am and after 8pm.

Define a function with type:
```ocaml
parrot_trouble : bool -> int -> bool option
```
where the first parameter tells whether the parrot is talking or not,
while the second parameter represents the current time (from 0 to 23).

The function returns `Some true` whenever the parrot is talking at a time
prohibited by the regulations.
The special value `None` must be used when the input time is outside the range
0..23.


Before starting the exercise, experiment with utop or [TryOCaml](try.ocamlpro.com) to learn how to use the `option` type:
```ocaml
None;;
Some 3;;
Some (2+2);;
```
