module Blackjhack.Participants.Player where

import           Blackjhack.Card
import           Blackjhack.Participant
import           Data.Char

newtype Player =
  Player
    { order :: Int
    }

instance Participant Player where
  decide _ (Hit, cs) = return Stand
    where
      askWhetherToHit :: IO Bool
      askWhetherToHit =
        putStrLn "カードを引きますか? (yes/no):" >> getLine >>= \s ->
          case map toLower s of
            "yes" -> return True
            "no"  -> return False
            _     -> askWhetherToHit
      tellCards :: [Card] -> IO ()
      tellCards cs = return ()
        --mapWithIndex (\(i, c) -> "* " ++ show (i + 1) ++ "番目に引いたカード: " + show c) cs
      mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
      mapWithIndex p = zipWith (curry p) [0 ..]
  expose _ = map Just
  name p = "Player-" ++ show (order p)
