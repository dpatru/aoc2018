import Data.Map.Strict as M (Map, insertLookupWithKey, insertWith, empty, foldl', filter)

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

squares (Claim n x y w h) = [(n, (x+i,y+j)) | i<-[0..w-1], j<-[0..h-1]]

findSpace sqMap = M.filter (== 1) idMap
  where idMap = M.foldl' (\m (ns,c)->foldl (\m n-> insertWith max n c m) m ns) empty sqMap
        
countOverlaps claims = (c, findSpace m)
  where (c, m) = foldl f (0, empty) [s | c<-claims,s<-squares c]
        f (c, m) (n, sq) = (maybe c (\(n,k)->if k==1 then c+1 else c) v, m')
          where (v, m') = insertLookupWithKey (\k (n2,c2) (n1,c1) ->(n2++n1, c1+c2)) sq ([n],1) m

main = do
  interact (show . countOverlaps . map parseClaim . lines)
  putStrLn ""
