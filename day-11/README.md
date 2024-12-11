make a cache dict of pre-calculation

this is a mathematics problem

f(0) = 1

f(x) = 2024 \* x, 1 <= x <= 9

f(x) = x[0] x[1], 10 <= x <= 99

let f(x, num) = the length of x after num blinks

f(0, n) = f(1, n - 1)

f(x, n) = f(x \* 2024, n - 1) if x % 2 != 0

...

---

when we get a new input value, if it's in the record, just replace it with f,
if not, blink it and do the same thing on its result

we extract f from the stones, and keep blink at the not-replaced stones

---

when initialized, we keep record of

f(1, 0), f(1, 1), f(1, 2), etc.

---

initially, we need to calculate:

f(41078, 75) + f(18, 75) + f(7, 75) + f(4785508, 75) + f(535256, 75) +
f(8154, 75) + f(447, 75)

and for each blink, we get instead:

f(41078, 1), ...

we can keep track of

we then do the blink, get every f(x, 74) after blink, then try to union them

---

$$
f(1, n) = f(2024, n-1) = f(20, n-2) + f(24, n-2) = 2 \* f(2, n-3) + f(0, n-3) +
f(4, n-3) = 2 * f(4048, n-4) + f(1, n-4) + f(8096, n-4)
$$

---

a map

```python

while number in mapping:
    return (mapping[number], num_blink - 1)

mapping = {
    1: (2024),
    2024: (20, 24),
    20: (2, 0)
    24: (2, 4)
}
```
