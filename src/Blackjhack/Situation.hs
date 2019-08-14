module Blackjhack.Situation
  ( Intention(..)
  , Situation
  , initialSituation
  ) where

import           Blackjhack.Card
import           Blackjhack.Participant
import           Blackjhack.Score

data Situation p =
  Situation
    { participant :: p
    , hand        :: [Card]
    , intention   :: Intention
    }

initialSituation :: (Participant a) => a -> Situation a
initialSituation participant = Situation participant [] Hit
