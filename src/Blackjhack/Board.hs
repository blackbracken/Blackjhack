module Blackjhack.Board
  ( Intention(..)
  , Board
  , Hand
  , Situation
  , prepareBoard
  ) where

import           Blackjhack.Card
import           Blackjhack.Participant
import           Blackjhack.Participants.Dealer
import           Blackjhack.Participants.Player
import           Data.Maybe

type Situation a = (a, Hand)

data Board =
  Board
    { deck             :: Deck
    , dealerSituation  :: Situation Dealer
    , playerSituations :: [Situation Player]
    }

prepareBoard :: Int -> Deck -> Board
prepareBoard x deck = Board deck (Dealer, initialHand) $ map (\order -> (Player order, initialHand)) [1 .. x]
  where
    initialHand :: Hand
    initialHand = (Hit, [])

tellBoard :: Board -> IO ()
tellBoard _ = putStrLn ""
