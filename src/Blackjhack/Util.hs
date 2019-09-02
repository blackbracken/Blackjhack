module Blackjhack.Util where

import           Control.Monad.State

headSafe :: [a] -> Maybe a
headSafe (x:_) = Just x
headSafe []    = Nothing

none :: (a -> Bool) -> [a] -> Bool
none predicate = not . any predicate

(..<) :: Int -> Int -> [Int]
(..<) start end = init [start .. end]

liftStateT :: (Monad m) => State a b -> StateT a m b
liftStateT = state . runState

