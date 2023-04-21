
firstAlternativeInit :: Num a => [a] -> [a]
firstAlternativeInit x = take (length x - 1) x

secondAlternativeInit :: Num a => [a] -> [a]
secondAlternativeInit x = reverse (tail (reverse x))

main :: IO()
main = do
  putStrLn . show $ firstAlternativeInit [1, 2, 3, 4, 5, 6]
-- [1, 2, 3, 4, 5]

  putStrLn . show $ secondAlternativeInit [1, 2, 3, 4, 5, 6]
-- [1, 2, 3, 4, 5]
