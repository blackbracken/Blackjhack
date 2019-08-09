module Main
  ( main
  ) where

import           Blackjhack.Board
import           Blackjhack.Card
import           Blackjhack.Participant
import           Control.Monad
import           System.Random

main :: IO ()
main = do
  seed <- getStdGen
  print "Hello"
