module Blackjhack.Participant
  ( Participant(..)
  , Intention(..)
  ) where

import           Blackjhack.Card

data Intention
  = Hit
  | Stand

class Participant a where
  decide :: a -> [Card] -> IO Intention
  hideHand :: a -> [Card] -> [Maybe Card]
  name :: a -> String
