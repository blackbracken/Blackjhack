module Blackjhack.Util where

import           Control.Monad.State

headSafe :: [a] -> Maybe a
headSafe (x:_) = Just x
headSafe []    = Nothing

none :: (a -> Bool) -> [a] -> Bool
none predicate = not . any predicate

(..<) :: Int -> Int -> [Int]
(..<) start end = init [start .. end]

(<++>) :: (Monad m) => [a] -> m [a] -> m [a]
(<++>) lefts = (<$>) (\rights -> lefts ++ rights)

skipMap :: Int -> (a -> a) -> [a] -> [a]
skipMap skip predicate mapped = take skip mapped ++ map predicate (drop skip mapped)

liftStateT :: (Monad m) => State a b -> StateT a m b
liftStateT = state . runState
