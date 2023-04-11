
-- Fibonacci
-- a : b : [] --> make a list starting [a, b, ...]
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

{-
1st : fib = 1 and tail fib = 1 --> zip --> (a, b) = (1, 1) --> a + b = 2 is pushed into list  ==> the list now [1,1,2]

2nd : fib = 1 (the second) and tail fib = 2 --> zip --> (a, b) = (1, 2) --> a + b = 3 ==> the list now [1, 1, 2, 3]

so on ....
-}

