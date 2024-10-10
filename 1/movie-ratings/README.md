# Movie ratings

Write a function of type:
```ocaml
movie_rating : int -> int -> int -> string
```
that takes the ratings of three critics for a movie (on a scale from 1 to 5) and returns a string indicating the overall rating:
- If all three critics give a rating of 5, return "Masterpiece".
- If two critics give a rating of 5 and the third gives at least a 4, return "Highly Recommended".
- If two critics give a rating of at least 4 and the third gives at least a 3, return "Recommended".
- Otherwise, return "Mixed Reviews".

The function must fail with an error if any of the inputs are not in the expected range.
