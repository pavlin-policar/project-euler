
import Data.List
import Data.Char
import Data.List.Split

primes :: [Int]
primes = sieve (2 : [3,5..])
  where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

-- Problem 3
primeFactors :: Int -> [Int]
primeFactors n = factor n primes
  where
    factor n (p:ps)
      | p * p > n       = [n]
      | n `mod` p == 0  = p : factor (n `div` p) (p:ps)
      | otherwise       = factor n ps

-- Problem 9
specialPythagoreanTriplet = product (head [[a,b,c] | c <- [1..500], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 1000])

-- Problem 10
p10 = sum $ takeWhile (\p -> p < 2000000) primes

-- Problem 12
triangleNumber :: Int -> Int
triangleNumber n =
  let candidates = scanl1 (+) [1..]
  in head $ filter (\p -> numberOk n p) candidates
  where numDivisors n = product $ map ((+1) . length) $ group $ primeFactors n
        numberOk len num = (numDivisors num) > len

-- Problem 14
collatzChain :: Integer -> [Integer]
collatzChain n
  | n == 1          = [1]
  | n `mod` 2 == 0  = [n] ++ (collatzChain $ n `div` 2)
  | otherwise       = [n] ++ (collatzChain $ 3 * n + 1)

p14 = fst $ maximumBy sortLn ls
  where
    ls = [(x, length (collatzChain x)) | x <- [1..1000000]]
    sortLn (n1, l1) (n2, l2)
      | l1 < l2   = LT
      | l1 > l2   = GT
      | l1 == l2  = compare n1 n2

-- Problem 17
one = ["one","two","three","four","five","six","seven","eight",
     "nine","ten","eleven","twelve","thirteen","fourteen","fifteen",
     "sixteen","seventeen","eighteen", "nineteen"]
ty = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
decompose x 
  | x == 0                       = []
  | x < 20                       = one !! (x - 1)
  | x >= 20 && x < 100           = ty !! (firstDigit x - 2) ++ decompose (x - firstDigit x * 10)
  | x < 1000 && x `mod` 100 == 0 = one !! (firstDigit x - 1) ++ "hundred"
  | x > 100 && x <= 999          = one !! (firstDigit x - 1) ++ "hundredand" ++ decompose (x - firstDigit x * 100)
  | x == 1000                    = "onethousand"
  where
    firstDigit x = digitToInt . head . show $ x
p17 = length . concatMap decompose $ [1..1000]

-- Problem 20
p20 = sum $ map digitToInt $ show $ product [1..100]

-- Problem 22
loadNames = do
  ls <- readFile "p22.txt"
  let sls = splitOn "," ls
  return $ sort $ map (filter (/= '"')) sls

getNumeric c = (ord c) - (ord 'A') + 1
getNameVal name = sum $ map getNumeric name
p22 = do
  ls <- loadNames
  let vals = map getNameVal ls
  return $ sum $ zipWith (*) vals [1..]

-- Problem 24
p24 = (sort (permutations "0123456789")) !! 999999

-- Problem 25
fibonacci :: [Integer]
fibonacci = scanl (+) 0 (1 : fibonacci)

-- Problem 45
p45 = reverse $ take 10 $ reverse $ show $ sum [x^x | x <- [1..1000]]

-- Problem 47
consec [] = []
consec (x1:x2:x3:xs) = if x1 == x2 - 1 && x2 == x3 - 1 then [x1] ++ [x2] ++ [x3]
  else consec [x2] ++ [x3] ++ xs

p47 = let candidates = ([x | x <- [1..], (length (nub (primeFactors x))) >= 4])
      in consec candidates
