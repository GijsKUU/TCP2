# Open questions

## Exercise 4
The Happy documentation states that for left-recursive you can use a constant space while right-recursive needs space proportional to the length of the list being parsed. This makes using left-recusive parsing more efficient, however you will have to reverse the resulting list to get it in the original order.
With parser combinators you cannot use left-recursion due to infinite recursion. Silmilarly to Happy, right-recursion with parser combinators uses proportional space to the length of the input list.

## Exercise 10
