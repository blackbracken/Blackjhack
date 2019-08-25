module Blackjhack.Util where

headSafe :: [a] -> Maybe a
headSafe (x:_) = Just x
headSafe []    = Nothing

none :: (a -> Bool) -> [a] -> Bool
none predicate = not . any predicate
