module Blackjhack.Participants.Dealer
  ( Dealer
  , dealer
  ) where

import           Blackjhack.Card
import           Blackjhack.Participant
import           Blackjhack.Situation

data Dealer =
  Dealer

dealer :: Dealer
dealer = Dealer

instance Participant Dealer where
  decide _ cards =
    return $
    case computeMaximumScoreWithoutBusted cards of
      Just x ->
        if x < 17
          then Hit
          else Stand
      Nothing -> Stand
  hideHand _ cards = map Just (take 1 cards) ++ replicate (length cards - 1) Nothing
  name _ = "Dealer"
