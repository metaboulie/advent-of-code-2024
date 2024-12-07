# day-six

## question 1

data direction = literal Up, Down, Left, Right

current_position = (x_0, y_0, direction)

get all obstacle positions and store in a suitable data structure

`passed_position` a unique list ( or a better data structure ) record all
passed positions

`moveForward` should do:

1. find the closest obstacle in the direction
    - if there aren't any, move to the edge and return
2. update `current_position` to the one before this obstacle and change
   direction
3. update `passed_positions`

return the length of `passed_positions`

## question 2

make a Queue of length 3 storing the position of every turing point, adding the
new turning point to this Queue in every turn and pop the first one is needed.

when the Queue has 3 elements, in every step, check whether the updated
position can form a rectangle with the 3 positions in the Queue.

the check function should be like:

if the guard turn 90 at the position before this position, she should be able
to meet the first position at the Queue

everytime the check function return true, acc += 1

---

Yeah, I thought rectangle was the only loop form, but it's not.

I failed loudly.
