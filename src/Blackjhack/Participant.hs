module Blackjhack.Participant
  ( Participant(..)
  , Intention(..)
  , isHit
  ) where

import           Blackjhack.Card

data Intention
  = Hit
  | Stand

isHit :: Intention -> Bool
isHit Hit = True
isHit _   = False

class Participant a where
  decide :: a -> [Card] -> IO Intention
  hideHand :: a -> [Card] -> [Maybe Card]
  name :: a -> String
