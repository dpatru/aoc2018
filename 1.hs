
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

stripPlus ('+':rest) = rest
stripPlus xs = xs

process ls = (show $ sum [(read $ stripPlus l) | l <- ls]) ++ "\n"
main = do
  interact (process . lines)
  putStrLn ""
  
