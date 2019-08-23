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
import           Data.Functor
import           Data.Maybe

data Board =
  Board
    { deck             :: Deck
    , dealerSituation  :: Situation Dealer
    , playerSituations :: [Situation Player]
    }

prepareBoard :: Int -> Deck -> Board
prepareBoard x deck = Board deck (initialSituation dealer) $ map (initialSituation . Player) [1 .. x]

-- TODO: annihilate this
play :: Board -> IO Board
play = return
  where
    turnNext :: (Participant a) => Deck -> Situation a -> IO (Deck, Situation a)
    turnNext deck situation =
      decideToPull situation <&> \decisionToPull ->
        if decisionToPull
          then addCardToHand deck situation
          else (deck, situation)
    addCardToHand :: (Participant a) => Deck -> Situation a -> (Deck, Situation a)
    addCardToHand deck situation@Situation {hand = hand} =
      case headSafe deck of
        Just pulledCard -> (tail deck, situation {hand = hand ++ [pulledCard]})
        Nothing -> (deck, situation)

isEnded :: Board -> Bool
isEnded Board {deck = deck, dealerSituation = dealerSituation, playerSituations = playerSituations} =
  canDecide dealerSituation || any canDecide playerSituations

--play :: StateT Board IO Board
tellBoard :: Board -> IO ()
tellBoard _ = putStrLn ""

type Result = Maybe Int

resultOf :: Situation a -> Result
resultOf Situation {hand = hand} = computeMaximumScoreWithoutBusted hand
