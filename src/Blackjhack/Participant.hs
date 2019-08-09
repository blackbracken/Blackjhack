module Blackjhack.Participant
  ( Participant(..)
  , Intention(..)
  , Hand
  ) where

import           Blackjhack.Card
import           Data.Char
import           Data.List
import           Data.Maybe

data Intention
  = Hit
  | Stand

type Hand = (Intention, [Card])

class Participant a where
  decide :: a -> Hand -> IO Intention
  decide _ (Stand, _) = return Stand
  expose :: a -> [Card] -> [Maybe Card]
  name :: a -> String
