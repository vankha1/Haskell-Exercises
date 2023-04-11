-- Write a function to generate all permutations of a string using recursion.
permutations :: String -> [String]
permutations [] = [[]]
permutations (x:xs) = [y | p <- permutations xs, y <- interleave x p]
  where interleave x [] = [[x]]
        interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- The semicolons before map is used for array
-- The map func will prepend the character y into each element (is a string) of array interleave
-- EX : interleave a "bc" 
-- a+bc -> abc. Separate first character, get b and prepend a to c through interleave x ys. Ater that call interleave a "c" --> we get ["ac"] and call interleave a "" --> get ["a"]
-- Go back interleave a "c" --> prepend c to each element ["a"] --> get ["ca"] --> concentenates --> get ["ac", "ca"]
-- Go back interleave a "bc" --> prepend b to each element ["ac", "ca"] --> get ["bac", "bca"] --> concentenates --> get ["abc", "bac", "bca"]

