
-- main = do
--   ls <- getContents
--   putStrLn $ show $ sum [read l :: Int | l <- lines ls]

-- main = do
--   interact (show . sum . map (read :: String -> Int) . lines)
--   putStrLn ""

-- main = do
--   interact (show . sum . map (read) . words)
--   putStrLn ""

-- import Text.Regex.PCRE.String
import Data.Set (Set, singleton, member, insert)

stripPlus ('+':rest) = rest
stripPlus xs = xs

extendList ls = ls ++ extendList ls -- lazy infinite list

process s f (c:cs) =
  if (f+c) `member` s
  then f+c
  else process (insert (f+c) s) (f+c) cs

main = do
  interact (show . process (singleton 0) 0 . extendList .
            map (read . stripPlus) . lines)
  putStrLn ""
  
