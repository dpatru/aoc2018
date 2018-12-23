
import Data.Map.Strict (Map, alter, empty, foldlWithKey, insertWith, (!))
import Data.List ()
import Data.Maybe (fromMaybe)

type Guard = Int

type TotalSleep = Map Guard Int
type MinuteSleep = Map Guard (Map Int Int)

type Timestamp = String

data GuardState = Begin | Asleep Guard Int | Awake Guard
                deriving (Show)
type State = (GuardState, TotalSleep, MinuteSleep)

startsWith :: Char -> String -> Bool
startsWith c s = c == head s

getMinutes :: String -> Int
getMinutes line = read $ line!!15 : line!!16 : []

getGuard :: String -> Guard
getGuard line = read $ tail $ (words line)!!3

process :: State -> String -> State
process (Begin, t, m) line = (Awake (getGuard line), t, m)
process (Awake g, t, m) line
  | k == "falls" = (Asleep g (getMinutes line), t, m)
  | k == "Guard" = (Awake (getGuard line), t, m)
  where k = (words line)!!2
process (Asleep g start, t, m) line = (Awake g, t', m')
  where t' = insertWith (+) g dur t
        m' = foldl (\m k -> alter (Just . insertWith (+) k 1 . fromMaybe empty) g m) m [start..start+dur-1]
        dur = getMinutes line - start

calculate :: State -> (Guard, Int, Int) -- part 1
calculate (Awake _, totals, minutes) = (g, m, g * m)
  where (_,g) = foldlWithKey (\(m,g) g' m' -> max (m,g) (m',g')) (0,-1) totals
        (_,m) = foldlWithKey (\(c,m) m' c' -> max (c,m) (c',m')) (0,-1) $ minutes!g
        
calculate _ = (-1,-1,-1)

calculate2 :: State -> (Guard, Int, Int) -- part 2
calculate2 (Awake _, totals, minutes) = (g, m, g * m)
  where (_, m, g) = foldlWithKey (\bestSoFar g minuteMap->foldlWithKey (\bestSoFar m c-> max bestSoFar (c, m, g)) bestSoFar minuteMap) (-1,-1,-1) minutes
        
main = do
  interact (show . calculate2 . foldl process (Begin, empty, empty) . lines)
  putStrLn ""
