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
  show r     = show $ fromEnum r

data Card =
  Card
    { suit :: Suit
    , rank :: Rank
    }

type Deck = [Card]

cards :: [Card]
cards = [Card s r | s <- [Spade .. Club], r <- [Ace .. King]]

genDeck :: (RandomGen r) => r -> Deck
genDeck = shuffle' cards $ length cards

pull :: Int -> Deck -> ([Card], Deck)
pull x deck = (take x deck, drop x deck)

calcPoint :: [Card] -> Maybe Int
calcPoint cs = headSafe . sortOn Down $ filter (<= 21) $ allCombinationsOfPoints cs
  where
    allCombinationsOfPoints :: [Card] -> [Int]
    allCombinationsOfPoints = foldr (\card rs -> nub $ concatMap (\r -> map (+ r) $ pointsOf card) rs) [0]
    pointsOf :: Card -> [Int]
    pointsOf = pointsFrom . rank

pointsFrom :: Rank -> [Int]
pointsFrom r
  | r == Ace = [1, 10]
  | r `elem` [King, Queen, Jack] = [10]
  | otherwise = [fromEnum r]
