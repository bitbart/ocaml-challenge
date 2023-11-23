# Squirrel play

Scrat the squirrel spend most of the day playing.
In particular, Scrat plays if the temperature is between 15 and 30 degrees (inclusive).
Only in summer, the upper limit is 35 instead of 30.

Given the type:
```ocaml
type season = Spring | Summer | Autumn | Winter
```
write a function with type:
```ocaml
squirrel_play : int -> season -> bool
```
that returns true if Scrat is playing, and false otherwise.

For instance, the following asserts must pass:
```ocaml
assert(squirrel_play 18 Winter = true);;
assert(squirrel_play 32 Spring = false);;
assert(squirrel_play 32 Summer = true);;
```