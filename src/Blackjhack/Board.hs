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
import           Blackjhack.Util
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

turnNext :: (Participant a) => Board -> Situation a -> IO (Board, Situation a)
turnNext board situation = return (board, situation)
  --dealerShouldPull <- decideToPull $ dealerSituation board
  where
    addCardToHand :: (Participant a) => Deck -> Situation a -> (Deck, Situation a)
    addCardToHand deck situation@Situation {hand = hand} =
      case headSafe deck of
        Just pulledCard -> (tail deck, situation {hand = hand ++ [pulledCard]})
        Nothing -> (deck, situation)

--play :: StateT Board IO Board
tellBoard :: Board -> IO ()
tellBoard _ = putStrLn ""

type Result = Maybe Int

resultOf :: Situation a -> Result
resultOf Situation {hand = hand} = computeMaximumScoreWithoutBusted hand
