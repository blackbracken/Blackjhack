module Blackjhack.Participant
  ( Participant
  ) where

import           Blackjhack.Card
import           Blackjhack.Game
import           Data.Maybe

class Participant a where
  decide :: a -> Hand a -> IO Intention
  expose :: a -> [Card] -> [Maybe Card]
