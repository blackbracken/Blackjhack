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
import           Data.List

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
    askWhetherToHit = do
      putStrLn "Are you sure you want to draw card (more)? [yes/no]:"
      reply <- getLine
      case map toLower reply of
        "yes" -> return True
        "no" -> return False
        _ -> do
          putStrLn "Try again; answer \"yes\" or \"no\"."
          askWhetherToHit
    tellCards :: Int -> [Card] -> IO ()
    tellCards score cards = do
      putStrLn $ "Your score: " ++ show score
      putStrLn $ "Your hand:" ++ intercalate ", " (map show cards)
    mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
    mapWithIndex action = zipWith (curry action) [0 ..]

hideHand :: Participant -> [Card] -> [Maybe Card]
hideHand Dealer cards = map Just (take 1 cards) ++ replicate (length cards - 1) Nothing
hideHand Player {} cards = map Just cards

name :: Participant -> String
name Dealer                 = "Dealer"
name Player {order = order} = "Player" ++ show order
