module Blackjhack.Participants.Dealer where

import           Blackjhack.Card
import           Blackjhack.Participant

data Dealer =
  Dealer

instance Participant Dealer where
  decide _ (Hit, cs) =
    return $
    case calcPoint cs of
      Just x ->
        if x < 17
          then Hit
          else Stand
      Nothing -> Stand
  expose _ cs = (Just $ head cs) : replicate (length cs - 1) Nothing
  name _ = "Dealer"
