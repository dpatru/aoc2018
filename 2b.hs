import Data.Set (Set, insert, empty, member)

type MySet = Set (String,String)
data State =  Begin MySet [String] 
           |  Check String String MySet [String]
           |  Found String String
           |  Fail
  deriving Show
  
check :: State -> State
check (Begin _ []) = Fail
check (Begin set (w:ws)) = check (Check w "" set ws)
check (Check "" _ set ws) = check (Begin set ws)
check (Check (x:xs) ys set ws) 
  | (xs, ys) `member` set = Found (reverse ys) xs
  | otherwise = check (Check xs (x:ys) ((xs,ys) `insert` set) ws)
  
main = do
  interact (show . (check . Begin empty) . lines)
  putStrLn ""
