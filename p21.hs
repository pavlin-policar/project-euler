
-- Problem 21
findDivisors n = [x | x <- [1..(n `div` 2)], n `mod` x == 0]

checkAmicable n = n == (sum $ findDivisors amicableNum) && n /= amicableNum
  where amicableNum = sum $ findDivisors n

p21 = sum $ filter checkAmicable [1..10000]

