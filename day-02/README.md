# day-two

Yeah, I only used haskell to resolve this puzzle.

## question 1

1. read into rows: `list[list[int]]`
2. diff on every internal list, get another `list[list[int]]`
3. write condition ( return 0 or 1 ) for this internal list:
    - if all elements are in `[1, 3]`
    - if all elements are in `[-3, -1]`
4. sum the list

## question 2

another checkrow function

1. remove the first element of the diffed row and check row
2. remove the last element of the diffed row and check row
3. for every 2 elements ( if there are more than 2 ), replace these 2 elements
   with their sum and check row
