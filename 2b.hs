import Data.Set (Set, member, insert, empty)

data Solution = NotFound
              | Found String String --prefix and suffix
              | Looking Set String String [String] --prefix, suffix, other words

search :: Solution -> Solution
search Looking s pre "" 
check :: Set -> [String] -> Maybe (String, String)
check s [] = Nothing
check s (w:ws) = maybe checkWord
  sort (x:xs) = sort [y | y <- xs, y < x] ++ [x] ++ sort [y | y <- xs, y >= x]
sort [] = []

checksum (x,y) = x * y


count (a, b, n, x) (y:ys)
  | x==y = count (a, b, n+1,x) ys
  | n == 2 && b > 0 = (1,1)
  | n == 2 = count (1, b, 1, y) ys
  | n == 3 && a > 0 = (1,1)
  | n == 3 = count (a, 1, 1, y) ys
  | otherwise = count (a, b, 1, y) ys
count (a,b,n,x) [] | n == 2 = (1,b)
                   | n == 3 = (a,1)
                   | otherwise = (a,b)


process (x,y) ys = (x+a, y+b)
  where (a,b) = count (0,0,0,'a') ys

main = do
  interact (show . checksum . foldl process (0,0) . map sort . lines)
  putStrLn ""

-- main = do
--   interact (unlines . map sort . lines)
--   putStrLn ""
