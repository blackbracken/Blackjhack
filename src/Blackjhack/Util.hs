module Blackjhack.Util where

headSafe :: [a] -> Maybe a
headSafe (x:_) = Just x
headSafe []    = Nothing
