module Blackjhack.Participants.Dealer where

import Blackjhack.Participant

instance Participant Dealer where
  decide _ (_, _, cs) = if  
