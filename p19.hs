
-- Problem 19
months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
leap = months !! 0 : 29 : drop 2 months

isLeap :: Int -> Bool
isLeap y = y `mod` 400 == 0 || y `mod` 100 /= 0 && y `mod` 4 == 0

years :: Int -> Int -> [[Int]]
years from to =
  let years = reverse $ [x | x <- [from..to]] -- Reverse, cons O(1)
  in reverse $ map (\y -> if isLeap y then leap else months) years

p19 =
  let m = foldl (++) [] $ years 1901 1999
  in length $ filter (== 0) $ scanl nm 1 m
  where
    nm p c = (p + c) `mod` 7

