module Main(main) where

import Blackjhack.Card
import Blackjhack.Game
import Blackjhack.Participant
import Blackjhack.Participants.Player
import Control.Monad
import System.Random

main :: IO ()
main = do
  seed <- getStdGen
  let deck = genDeck seed
  putStrLn $ show $ fst $ prepareBoard deck [Player]
