import Data.List (sort)

secondLargest :: (Ord a) => [a] -> a
secondLargest xs
  | length xs < 2 = error "List must have at least two elements"
  | otherwise = head . tail . reverse $ sort xs