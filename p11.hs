
import Data.List
import Data.List.Split

getGrid = do
  ls <- readFile "p11.txt"
  let splitList = splitOn "\n" ls
  let strList = map (splitOn " ") splitList
  let list = map (map (\p -> read p :: Int)) strList
  return $ list

chunks :: Int -> [a] -> [[a]]
chunks n l
  | length chunk < n = []
  | otherwise = chunk : chunks n (tail l)
  where chunk = take n l

horizontal :: [[Int]] -> [[Int]]
horizontal = concatMap (chunks 4)

vertical :: [[Int]] -> [[Int]]
vertical = horizontal . transpose

diagonal :: [[Int]] -> [[Int]]
diagonal = concatMap (chunks 4) . transpose . zipWith drop [0..]

southEast = concatMap diagonal . tails
southWest = southEast . map reverse

largestProduct :: [[Int]] -> Int
largestProduct = maximum . map product

p11 = do
  ls <- getGrid
  return $ largestProduct $ concatMap ($ ls) [horizontal, vertical, southEast, southWest]


