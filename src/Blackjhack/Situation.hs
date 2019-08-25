module Blackjhack.Situation
  ( Situation(..)
  , initialSituation
  , canDecide
  , decideToPull
  ) where

import           Blackjhack.Card
import           Blackjhack.Participant
import           Blackjhack.Util
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

playTurn :: Deck -> Situation -> IO (Deck, Situation)
playTurn deck situation@Situation {participant = participant} =
  decideToPull situation <&> \decision ->
    if decision
      then addCardToHand deck situation
      else (deck, situation)
  where
    addCardToHand :: Deck -> Situation -> (Deck, Situation)
    addCardToHand deck situation@Situation {hand = hand} =
      case headSafe deck of
        Just pulledCard -> (tail deck, situation {hand = hand ++ [pulledCard]})
        Nothing -> (deck, situation)

canDecide :: Situation -> Bool
canDecide Situation {intention = Stand} = False
canDecide Situation {hand = hand} = isJust $ computeMaximumScoreWithoutBusted hand

decideToPull :: Situation -> IO Bool
decideToPull situation@Situation {participant = participant, hand = hand} =
  if canDecide situation
    then isHit <$> decide participant hand
    else return False
