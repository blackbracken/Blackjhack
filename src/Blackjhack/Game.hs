module Blackjhack.Game (
    Participant(..),

    prepareBoard
) where

import Blackjhack.Card
import Data.Maybe
import System.Random

data Intention = Hit | Stand
data Participant = Player | Dealer

type Board = (Deck, [(Participant, [Card])])

class BlackjhackPlayer a where
  decide :: a -> Board -> IO Intention
  expose :: a -> [Card] -> [Maybe Card]

prepareBoard :: Deck -> [Participant] -> Board
prepareBoard d ps = (d, map (\p -> (p, [])) ps)
