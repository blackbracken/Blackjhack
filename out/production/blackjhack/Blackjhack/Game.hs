module Blackjhack.Game (
    Intention(..),
    Board,
    Hand,

    prepareBoard
) where

import Blackjhack.Card
import Blackjhack.Participant
import Data.Maybe

data Intention = Hit | Stand

type Board = (Deck, Hand Dealer, [Hand Player])
type Hand a = Hand
  { participant :: Participant a
  , intention   :: Intention,
  , cards       :: [Card])
  }

prepareBoard :: (Participant a) => Deck -> [a] -> Board
prepareBoard d ps = (d, Hit, map (\p -> (p, [])) ps)
