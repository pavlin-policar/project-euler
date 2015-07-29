
import Data.List.Split

getNums = do
  ls <- readFile "p18.txt"
  return $ map (map (\n -> read n :: Int)) $ map (splitOn " ") $ lines ls

getMax ls =
  head $ foldr1 g ls
  where
    acc x y z = x + max y z
    g xs ys = zipWith3 acc xs ys $ tail ys

p18 = do
  ls <- getNums
  return $ getMax ls

