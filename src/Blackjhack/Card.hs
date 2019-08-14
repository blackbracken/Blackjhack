module Blackjhack.Card
  ( Suit
  , Rank
  , Card
  , Deck
  , genDeck
  , pull
  , calcPoint
  ) where

import           Blackjhack.Util
import           Data.List
import           Data.Ord
import           System.Random
import           System.Random.Shuffle

data Suit
  = Spade
  | Diamond
  | Heart
  | Club
  deriving (Eq, Enum)

data Rank
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Eq, Enum)

instance Show Suit where
  show Spade   = "S"
  show Diamond = "D"
  show Heart   = "H"
  show Club    = "C"

instance Show Rank where
  show Ace   = "Ace"
  show King  = "King"
  show Queen = "Queen"
  show Jack  = "Jack"
  show r     = show $ fromEnum r + 1

data Card =
  Card
    { suit :: Suit
    , rank :: Rank
    }

type Deck = [Card]

cards :: [Card]
cards = [Card suit rank | suit <- [Spade .. Club], rank <- [Ace .. King]]

genDeck :: (RandomGen r) => r -> Deck
genDeck = shuffle' cards $ length cards

pull :: Int -> Deck -> ([Card], Deck)
pull x deck = (take x deck, drop x deck)

calcPoint :: [Card] -> Maybe Int
calcPoint cards = headSafe . sortOn Down $ filter (<= 21) $ allCombinationsOfPoints cards
  where
    allCombinationsOfPoints :: [Card] -> [Int]
    allCombinationsOfPoints = foldr combinePoint [0]
    combinePoint :: Card -> [Int] -> [Int]
    combinePoint card acc = nub $ concatMap (\x -> map (+ x) $ pointsOf card) acc
    pointsOf :: Card -> [Int]
    pointsOf = pointsFrom . rank

pointsFrom :: Rank -> [Int]
pointsFrom rank
  | rank == Ace = [1, 10]
  | rank `elem` [King, Queen, Jack] = [10]
  | otherwise = [fromEnum rank + 1]
