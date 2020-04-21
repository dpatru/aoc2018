module Lib
    ( someFunc
    ) where

import Data.Map (Map, (!), insert, insertWith, member )
import Data.List (sort)
import Data.Maybe (isNothing, fromJust)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- We want to count the size of the regions belonging to each point
-- and return the largest one which is not on a border.
type Point = (Int, Int)

distance :: Point -> Point -> Int
distance (a,b) (c,d) = abs (a-c) + abs (b-d)

type Region = (Point, Point) -- starting point and distance vector
size :: Region -> Int
size (_, (x,y)) = (x+1)*(y+1)
extents :: Region -> [Point] -- the other boundary points
extents ((x,y), (a,b))
  | a == 0 && b == 0 = []
  | a == 0 = [(x,y+b)]
  | x == 0 = [(x+a,y)]
  | otherwise = [(x+a,y),(x,y+x),(x+a,y+b)]

data S = S { points :: [Point]
           , minPoint :: Point
           , maxPoint :: Point
           , area :: Map Point Int
           , regions :: [Region]
           , owner :: Map Point (Maybe Point)
           , border :: Map Point Bool
           }

onBorder :: Point -> S -> Bool
onBorder (x,y) s = x == (fst $ minPoint s)
                   || x == (fst $ maxPoint s)
                   || y == (snd $ minPoint s)
                   || y == (snd $ maxPoint s)

updateCaches :: S -> Point -> S
updateCaches s p = s { owner = if p `member` owner s then owner s
                               else insert p o $ owner s }
  where (d1,p1):(d2,_):_ = sort [(distance p a, a) | a<-points s]
        o = if d1 == d2 then Nothing else Just p1
        
step :: S -> S
step s
  | null $ regions s = s -- no more regions to process
  | isNothing o -- p belongs to nobody, move to the next point
  = step 
    $ foldl updateCaches (s { regions = slideRegions ++ rs })
    $ map fst slideRegions
  | all (== o) [(owner s)!e | e <-extents r]
  = step $ s { area = insertWith (+) o' (size r) $ area s
             , regions = rs
             , border = insertWith (||) o' (onBorder o' s) $ border s
             }
  | otherwise -- no single owner, split up the region
  = step
    $ foldl updateCaches (s {regions = splitRegions ++ rs})
    $ map fst splitRegions
  where
    r@(p@(x,y), d@(dx,dy)): rs = regions s -- pattern match regions
    o = (owner s) ! p
    slideRegions = [((x+1,y),(dx-1,0))
                   ,((x,y+1),(0,dy-1))
                   ,((x+1,y+1),(dx-1,dy-1))
                   ]
    o' = fromJust o
    dx2 = dx `quot` 2
    dy2 = dy `quot` 2
    splitRegions = [(p,(dx2,dy2))
                   ,((x+dx2,y),(dx-dx2,dy2))
                   ,((x,y+dy2),(dx2,dy-dy2))
                   ,((x+dx2,y+dy2),(dx-dx2,dy-dy2))
                   ]
