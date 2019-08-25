{-# OPTIONS -Werror=incomplete-patterns #-}

module Blackjhack.Participant
  ( Participant(Dealer, Player, order)
  , decide
  , hideHand
  , name
  , Intention(..)
  , isHit
  ) where

import           Blackjhack.Card
import           Data.Char

data Participant
  = Dealer
  | Player
      { order :: Int
      }

data Intention
  = Hit
  | Stand

isHit :: Intention -> Bool
isHit Hit   = True
isHit Stand = False

decide :: Participant -> [Card] -> IO Intention
decide Dealer cards =
  return $
  case computeMaximumScoreWithoutBusted cards of
    Just x ->
      if x < 17
        then Hit
        else Stand
    Nothing -> Stand
decide Player {} cards = return Stand
  where
    askWhetherToHit :: IO Bool
    askWhetherToHit =
      putStrLn "Are you sure you want to draw card (more)? [yes/no]:" >> getLine >>= \reply ->
        case map toLower reply of
          "yes" -> return True
          "no"  -> return False
          _     -> askWhetherToHit
    tellCards :: [Card] -> IO ()
    tellCards cards = return ()
                              --mapWithIndex (\(i, c) -> "* " ++ show (i + 1) ++ "番目に引いたカード: " + show c) cs
    mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
    mapWithIndex action = zipWith (curry action) [0 ..]

hideHand :: Participant -> [Card] -> [Maybe Card]
hideHand Dealer cards = map Just (take 1 cards) ++ replicate (length cards - 1) Nothing
hideHand Player {} cards = map Just cards

name :: Participant -> String
name Dealer                 = "Dealer"
name Player {order = order} = "Player-" ++ show order
