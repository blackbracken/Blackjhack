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
    dealInitialHand situations =
      forM situations $ \situation -> do
        deck <- get
        return $ situation {hand = take 2 deck}
    playPlayers :: [Situation] -> StateT Deck IO [Situation]
    playPlayers [] = return []
    playPlayers situations =
      let dealerSituation = take 1 situations
          playerSituations =
            forM (drop 1 situations) $ \situation -> do
              deck <- get
              decision <- lift $ decideToPull situation
              return $
                if decision
                  then evalState (pull 1 situation) deck
                  else situation
       in dealerSituation <++> playerSituations
