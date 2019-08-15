module Blackjhack.Board
  ( Board
  , Situation
  , prepareBoard
  ) where

import           Blackjhack.Card
import           Blackjhack.Participant
import           Blackjhack.Participants.Dealer
import           Blackjhack.Participants.Player
import           Blackjhack.Situation
import           Control.Monad.State
import           Data.Maybe

data Board =
  Board
    { deck             :: Deck
    , dealerSituation  :: Situation Dealer
    , playerSituations :: [Situation Player]
    }

prepareBoard :: Int -> Deck -> Board
prepareBoard x deck = Board deck (initialSituation dealer) $ map (initialSituation . Player) [1 .. x]

--play :: StateT Board IO Board
tellBoard :: Board -> IO ()
tellBoard _ = putStrLn ""

type Result = Maybe Int

resultOf :: Situation a -> Result
resultOf (_, hand, _) = computeMaximumScoreWithoutBusted hand
