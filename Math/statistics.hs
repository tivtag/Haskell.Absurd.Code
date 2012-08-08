import Data.List
import Data.Function

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / genericLength xs

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

mode :: Ord a => [a] -> a
mode xs = head . last $ sortBy (compare `on` length) (groupBy (==) xs)

variance :: (RealFrac a, Fractional b) => [a] -> b
variance xs = mean $ map ((^2) . subtract (mean xs)) xs

stddev :: RealFloat a => [a] -> a
stddev = sqrt . variance

stdscore :: RealFloat a => a -> [a] -> a
stdscore x xs = (x - mean xs) / stddev xs

-- The number of ways to choose k elements from an n-element set.
binorm :: Int -> Int -> Int
binorm n k = product [k+1..n] `div` product [1..n-k]

factorial :: Int -> Int
factorial n = product [1..n]
