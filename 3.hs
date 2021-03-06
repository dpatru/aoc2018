import Data.Map.Strict (Map, insertLookupWithKey, empty)

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

squares (Claim id x y w h) = [(x+i,y+j) | i<-[0..w-1], j<-[0..h-1]]
countOverlaps claims = c
  where (c, s) = foldl f (0, empty) [s | c<-claims,s<-squares c]
        f (c, m) sq = (maybe c (\k->if k==1 then c+1 else c) v, m')
          where (v, m') = insertLookupWithKey (\k n o->n+o) sq 1 m

main = do
  interact (show . countOverlaps . map parseClaim . lines)
  putStrLn ""
