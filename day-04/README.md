# day-four

## question 1

1. subsitute `XMAS` with `1234` respectively

then we need to search all overlapped `1234` and `4321` horizontally,
vertically and diagonally

243112434212412424341414124

how many

consider `1234`

for a row:

-   every `search_4` should return a 1

-   `search_x` shuold call both `search_x` and `search_x+1` on the after match

-   start from run `search_1` on the whole row

---

algorithm 2

for all X, for all M after_match, for all A after_match, for all S after_match,
acc += 1

acc should be a global state

> [!caution]
>
> i think haskell is not suitable for this puzzle
