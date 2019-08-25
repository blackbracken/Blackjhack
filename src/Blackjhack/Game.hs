module Blackjhack.Game
  ( Board(..)
  , initialBoard
  ) where

import           Blackjhack.Card
import           Blackjhack.Participant
import           Blackjhack.Situation
import           Blackjhack.Util
import           Control.Monad.State
import           Data.Functor
import           Data.Maybe

data Board =
  Board
    { deck       :: Deck
    , situations :: [Situation]
    }

initialBoard :: Int -> Deck -> Board
initialBoard x deck = Board deck $ initialSituation Dealer : map (initialSituation . Player) [1 .. x]

play :: StateT Board IO Board
play = get

isEnded :: Board -> Bool
isEnded Board {deck = deck, situations = situations} = null deck || none canDecide situations

tellBoard :: Board -> IO ()
tellBoard _ = putStrLn ""

type Result = Maybe Int

resultOf :: Situation -> Result
resultOf Situation {hand = hand} = computeMaximumScoreWithoutBusted hand
