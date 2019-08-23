module Blackjhack.Situation
  ( Intention(..)
  , Situation(..)
  , initialSituation
  , canDecide
  , decideToPull
  ) where

import           Blackjhack.Card
import           Blackjhack.Participant
import           Data.Maybe

data Situation p =
  Situation
    { participant :: p
    , hand        :: [Card]
    , intention   :: Intention
    }

initialSituation :: (Participant a) => a -> Situation a
initialSituation participant = Situation {participant = participant, hand = [], intention = Hit}

canDecide :: (Participant a) => Situation a -> Bool
canDecide Situation {intention = Stand} = False
canDecide Situation {hand = hand} = isJust $ computeMaximumScoreWithoutBusted hand

decideToPull :: (Participant a) => Situation a -> IO Bool
decideToPull situation@Situation {participant = participant, hand = hand} =
  if canDecide situation
    then isHit <$> decide participant hand
    else return False
