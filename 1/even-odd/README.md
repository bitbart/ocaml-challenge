# Even and odd

Write a function with the following type:
```ocaml
is_even : int -> bool
```
such that ``is_even x`` is true iff x is even. *Bonus*: do not use the if-then-else construct.

Now, consider a simple game between two players, say A and B, with the following rules:
- A and B choose a number between 1 and 5 (any other number is considered incorrect);
- the winner is A if she played a correct number, and the sum of the two numbers is even;
- the winner is B if he played a correct number, and the sum of the two numbers is odd;
- if one player has chosen a correct number and the other has not, then the first one wins;
- if both players have chosen an incorrect number, then no one wins.

Write a function to determine the winner of the game, with type:
```ocaml
val win : int -> int -> int
```
such that ``win a b`` evaluates to:
- 1, if the first player wins (i.e., the one who played the number `a`);
- -1, if the second player wins;
- 0, if no one wins.
