# Open questions

## Exercise 4
The Happy documentation states that for left-recursive you can use a constant space while right-recursive needs space proportional to the length of the list being parsed. This makes using left-recusive parsing more efficient, however you will have to reverse the resulting list to get it in the original order, increasing time complexity.
With parser combinators you cannot use left-recursion due to infinite recursion. Silmilarly to Happy, right-recursion with parser combinators uses proportional space to the length of the input list.

## Exercise 10
For the size of the stack it matters a little when the recursive call is made. When it is made before the end of the command sequence, the commands that hadn't been carried out yet will remain on the stack, meaning if this happens recursively the stack will slowly grow larger. If the recursive call is made at the end of the command sequence, all commands for a rule are already carried out so the size of the stack will just return to the size it was before carrying out this sequence of commands.