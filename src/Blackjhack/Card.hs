module Blackjhack.Card (
    Suit,
    Rank,

    Card,
    Deck,
    
    genDeck,
    pull
) where

import System.Random
import System.Random.Shuffle

data Suit = Spade
            | Diamond
            | Heart
            | Club
            deriving (Eq, Enum)

data Rank = Ace
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

type Card = (Suit, Rank)
type Deck = [Card]

cards :: [Card]
cards = [ (s, r) | s <- [Spade .. Club], r <- [Ace .. King] ]

genDeck :: (RandomGen r) => r -> Deck
genDeck = shuffle' cards $ length cards

pull :: Int -> Deck -> ([Card], Deck)
pull x deck = (take x deck, drop x deck)
