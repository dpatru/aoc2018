import Data.Set (Set, insert, empty, member)

type MySet = Set (Int, Int)
data Claim = Claim Int Int Int Int Int -- Id, x, y, w, 
  deriving (Show, Read)

isDigit x = '0' <= x && x <= '9'

parseNumbers :: String -> [Int]
parseNumbers xs = start xs []
  where start (x:xs) ns | isDigit x = num [x] xs ns
                        | otherwise = start xs ns
        start [] ns = ns
        num y (x:xs) ns | isDigit x = num (x:y) xs ns
                        | otherwise = start xs $ addn ns y
        num y [] ns = addn ns y
        addn ns y = ns ++ [read $ reverse y]
        
trimHash ('#':s) = s
trimHash s = s

parseClaim line = Claim i x y w h
  where [i,x,y,w,h] = parseNumbers line

squares (Claim id x y w h) = [(x+i,y+j) | i<-[0..w], j<-[1..h]]
countOverlaps claims = c
  where (c, s) = foldl f (0, empty) [s | c<-claims,s<-squares c]
        f (c, set) sq | sq `member` set = (c+1,set)
                      | otherwise = (c, sq `insert` set) 
main = do
  interact (show . countOverlaps . map parseClaim . lines)
  putStrLn ""
