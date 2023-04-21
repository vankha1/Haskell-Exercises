{-
Write a function longestCommonPrefix that takes an array of strings as input and returns the longest common prefix of all the strings in the array. -}

longestCommonPrefix :: [String] -> String
longestCommonPrefix [] = ""
longestCommonPrefix (x:xs) = foldl commonPrefix x xs
  where
    commonPrefix acc str = map fst . takeWhile (uncurry (==)) $ zip acc str
{- 
The zip acc str --> creates a list of pairs. Each pair contains a character from the acc string and the corresponding character from the str string.

The takeWhile (uncurry (==)) expression takes elements from the list of pairs while the characters in each pair are equal. The uncurry (==) function --> compare the characters in each pair. This function returns True if the characters are equal and False otherwise.

The map fst expression extracts the {first} character from each pair in the resulting list and creates a new string. This new string represents the common prefix of the acc and str strings.

For example, if we call the commonPrefix function with the arguments "flower" and "flow", it will zip these two strings together to create a list of pairs: [('f', 'f'), ('l', 'l'), ('o', 'o'), ('w', 'w'), ('e', 'f'), ('r', '\0')]. The takeWhile (uncurry (==)) expression will take elements from this list while the characters in each pair are equal. In this case, only the first four pairs have equal characters, so the result is [('f', 'f'), ('l', 'l'), ('o', 'o'), ('w', 'w')]. The map fst expression will then extract the first character from each pair and create a new string: "flow". -}