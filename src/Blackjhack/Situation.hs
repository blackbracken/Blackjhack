module Blackjhack.Situation
  ( Situation(..)
  , initialSituation
  , decideToPull
  , pull
  , playTurn
  ) where

import           Blackjhack.Card
import           Blackjhack.Participant
import           Blackjhack.Util
import           Control.Monad.State
import           Data.Functor           ((<&>))
import           Data.Maybe

data Situation =
  Situation
    { participant :: Participant
    , hand        :: [Card]
    , intention   :: Intention
    }

initialSituation :: Participant -> Situation
initialSituation participant = Situation {participant = participant, hand = [], intention = Hit}

decideToPull :: Situation -> IO Bool
decideToPull situation@Situation {participant = participant, hand = hand} =
  if canDecide situation
    then isHit <$> decide participant hand
    else return False
  where
    canDecide :: Situation -> Bool
    canDecide Situation {intention = Stand} = False
    canDecide Situation {hand = hand} = isJust $ computeMaximumScoreWithoutBusted hand

pull :: Int -> Situation -> State Deck Situation
pull pulledNumber situation@Situation {hand = hand} = do
  deck <- get
  let pulledCard = take pulledNumber deck
  put $ drop pulledNumber deck
  return $ situation {hand = hand ++ pulledCard}

playTurn :: Situation -> StateT Deck IO Situation
playTurn situation = do
  deck <- get
  decisionToPull <- liftIO $ decideToPull situation
  if decisionToPull
    then state . runState $ pull 1 situation
    else return situation
