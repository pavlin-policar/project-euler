
import Data.List

-- Problem 23
getDivisors n = [x | x <- [1..(n `div` 2)], n `mod` x == 0]
isAbundant n = (sum $ getDivisors n) > n
abundants = filter isAbundant [1..]

isSumOfAbundants n = any (\k -> isAbundant (n - k)) (takeWhile (< n) abundants)

p23 = sum $ filter (not . isSumOfAbundants) [1..28123]
