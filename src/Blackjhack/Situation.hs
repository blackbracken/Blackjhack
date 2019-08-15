module Blackjhack.Situation
  ( Intention(..)
  , Situation(..)
  , initialSituation
  ) where

import           Blackjhack.Card
import           Blackjhack.Participant

type Situation p = (p, [Card], Intention)

initialSituation :: (Participant a) => a -> Situation a
initialSituation participant = (participant, [], Hit)

turnNext :: (Participant a) => Situation a -> IO (Situation a)
turnNext situation@(_, _, Stand)          = return situation
turnNext situation@(participant, hand, _) = return situation
 -- case computeMaximumScoreWithoutBusted hand of
 --   Just _ ->
 --     intention <- decide participant hand
 --     case intention of
 --       Hit -> situation
 --       Stand -> situation
 --   Nothing -> return situation
 -- where
