module Blackjhack.Card
  ( Suit
  , Rank
  , Card
  , Deck
  , genDeck
  , computeMaximumScoreWithoutBusted
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

instance Show Card where
  show Card {suit = suit, rank = rank} = "[ " ++ show suit ++ " / " ++ show rank ++ " ]"

type Deck = [Card]

cards :: [Card]
cards = [Card suit rank | suit <- [Spade .. Club], rank <- [Ace .. King]]

genDeck :: (RandomGen r) => r -> Deck
genDeck = shuffle' cards $ length cards

computeMaximumScoreWithoutBusted :: [Card] -> Maybe Int
computeMaximumScoreWithoutBusted = headSafe . sortOn Down . filterNotBusted . allCombinationsOfScore
  where
    allCombinationsOfScore :: [Card] -> [Int]
    allCombinationsOfScore = foldr (+>) [0]
    (+>) :: Card -> [Int] -> [Int]
    (+>) card acc = nub $ concatMap (\x -> map (+ x) $ scoreOf card) acc
    filterNotBusted :: [Int] -> [Int]
    filterNotBusted = filter (<= 21)
    scoreOf :: Card -> [Int]
    scoreOf = scores . rank

scores :: Rank -> [Int]
scores rank
  | rank == Ace = [1, 10]
  | rank `elem` [King, Queen, Jack] = [10]
  | otherwise = [fromEnum rank + 1]
