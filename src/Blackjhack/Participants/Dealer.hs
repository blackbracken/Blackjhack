module Blackjhack.Participants.Dealer where

import           Blackjhack.Card
import           Blackjhack.Participant

data Dealer =
  Dealer

instance Participant Dealer where
  decide _ (Hit, cards) =
    return $
    case calcPoint cards of
      Just x ->
        if x < 17
          then Hit
          else Stand
      Nothing -> Stand
  expose _ cards = map Just (take 1 cards) ++ replicate (length cards - 1) Nothing
  name _ = "Dealer"
