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

play :: Deck -> Int -> IO [Situation]
play deck numberOfPlayers = return $ evalState (dealInitialHand $ genSituations numberOfPlayers) deck
  where
    genSituations :: Int -> [Situation]
    genSituations number = initialSituation Dealer : map (initialSituation . Player) (1 ..< number)
    dealInitialHand :: [Situation] -> State Deck [Situation]
    dealInitialHand situations = do
      put deck
      forM situations $ \situation -> do
        deck <- get
        put $ drop 2 deck
        return $ situation {hand = take 2 deck}
