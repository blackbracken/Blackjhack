module Main(main) where

import Blackjhack.Card
import Control.Monad
import System.Random

main :: IO ()
main = do
  seed <- getStdGen
  putStrLn $ show $ fst $ pull 1 $ genDeck seed

