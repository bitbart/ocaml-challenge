# Exam Committee

You are tasked with writing a function that determines whether an exam candidate passes or fails based on the votes of a three-member committee. 
Each committee member can vote using one of four possible values:
- `StrongReject`
- `WeakReject`
- `WeakAccept`
- `StrongAccept`

The exam is passed if at least two committee members support acceptance (either weakly or strongly), unless the third member gives a strong reject, in which case the candidate fails.

Write a function with type:
```ocaml
decide_exam : vote -> vote -> vote -> bool
```
that takes as input three votes and returns a bool indicating whether the candidate passes (true) or fails (false).
