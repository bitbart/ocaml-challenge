type season = Spring | Summer | Autumn | Winter;;
              
let squirrel_play t s = 
  t>=15 && t<=(if s=Summer then 35 else 30)
;;

assert(squirrel_play 18 Winter = true);;
assert(squirrel_play 32 Spring = false);;
assert(squirrel_play 32 Summer = true);;
