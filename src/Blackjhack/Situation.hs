module Blackjhack.Situation
  ( Intention(..)
  , Situation(..)
  , initialSituation
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

decideToPull :: (Participant a) => Situation a -> IO Bool
decideToPull Situation {intention = Stand} = return False
decideToPull situation@Situation {participant = participant, hand = hand} =
  case computeMaximumScoreWithoutBusted hand of
    Just _  -> isHit <$> decide participant hand
    Nothing -> return False
