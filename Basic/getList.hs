-- Get list from start index to end index of a list.

getElementBetween :: Int->Int->[a]->[a]

getElementBetween start end xs = take (end - start + 1) (drop start xs)

