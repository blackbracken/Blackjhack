module Blackjhack.Participants.Player where

import           Blackjhack.Card
import           Blackjhack.Participant
import           Data.Char

newtype Player =
  Player
    { order :: Int
    }

instance Participant Player where
  decide _ (Hit, cards) = return Stand
    where
      askWhetherToHit :: IO Bool
      askWhetherToHit =
        putStrLn "Are you sure you want to draw card (more)? [yes/no]:" >> getLine >>= \reply ->
          case map toLower reply of
            "yes" -> return True
            "no"  -> return False
            _     -> askWhetherToHit
      tellCards :: [Card] -> IO ()
      tellCards cards = return ()
        --mapWithIndex (\(i, c) -> "* " ++ show (i + 1) ++ "番目に引いたカード: " + show c) cs
      mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
      mapWithIndex action = zipWith (curry action) [0 ..]
  expose _ = map Just
  name participant = "Player-" ++ show (order participant)
