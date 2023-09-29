# Morra

Consider the type:
```ocaml
type winner = Player | Computer | Tie ;;
```

Write a function with type:
```ocaml
win : int * int -> (int * int) * winner
```
which determines the winner of a round of the Morra game.
The parameters `(hp,gp)` in a function call `win (hp,gp)`
represent, respectively, the hand and the guess of the player
The function randomly generates another pair of numbers:
- `hc`, representing the hand of the computer (in the range 0..5);
- `hg`, representing the guess of the computer (in the range 0..10);

The winner is determined as follows:
- if the guess of the player equals the sum of the two hands, but that of the computer does not, then the winner is the player;
- otherwise, if the computer guesses the sum of the two hands, then the winner is the computer;
- otherwise, there is a tie.

Besides the winner, the returned value must return the hand and the guess of the computer.
For example:
```ocaml
win(2,5);;
- : (int * int) * winner = ((0, 1), Tie)

win(2,5);;
- : (int * int) * winner = ((4, 6), ComputerWins)

win(2,5);;
- : (int * int) * winner = ((3, 4), PlayerWins)
```