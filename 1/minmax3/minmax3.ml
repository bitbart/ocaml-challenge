let minmax3 a b c =
  let max2 x y = if (x < y) then y else x in
  let min2 x y = if (x < y) then x else y in
   (  min2 a (min2 b c), max2 a (max2 b c)  );;

minmax3 1 2 3;;
