module Blackjhack.Participants.Player where

import           Blackjhack.Card
import           Blackjhack.Game
import           Blackjhack.Participant

instance Participant Player where
  decide _ (_, _, cs) = tellCards cs >> return Stand
  expose _ = map Just

tellCards :: [Card] -> IO ()
tellCards cs = map putStrLn $ mapWithIndex (\i c -> "* " ++ (i + 1) ++ "番目に引いたカード: " + show c) cs
  where
    mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
    mapWithIndex p = zipWith (curry p) [0 ..]
