module Blackjhack.Game
  ( play
  ) where

import           Blackjhack.Card
import           Blackjhack.Participant
import           Blackjhack.Situation
import           Blackjhack.Util
import           Control.Monad.State
import           Data.Functor
import           Data.Maybe

type Result = [Situation]

play :: Deck -> Int -> StateT Deck IO [Situation]
play deck numberOfPlayers = return $ genSituations numberOfPlayers
  where
    genSituations :: Int -> [Situation]
    genSituations number = initialSituation Dealer : map (initialSituation . Player) (1 ..< number)
