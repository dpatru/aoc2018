import Data.Char (isLetter, toLower)

opp x y | x == y = False
        | toLower x == toLower y = True
        | otherwise = False

compact :: String -> String
compact "" = ""
compact (x:[]) = (x:[])
compact (x:y:zs) | opp x y = compact zs
                 | otherwise = x: compact (y:zs)

process :: String -> String
process s = if s1 == s2 then s2 else process s2
  where s1 = compact s
        s2 = compact s1

removeBest :: String -> Int
removeBest s = minimum [length $ process $ filter ((/=) c . toLower) s | c <- ['a'..'z']]
main = do
  -- interact (show . length . process . filter isLetter) -- part 1
  interact (show . removeBest . process . filter isLetter) -- part 2
  putStrLn ""
