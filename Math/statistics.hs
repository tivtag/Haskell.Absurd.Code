import Data.List
import Data.Function

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / genericLength xs

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

mode :: Ord a => [a] -> a
mode xs = head . last $ sortBy (compare `on` length) (groupBy (==) xs)

-- The variance is a measure of how far a set of numbers is spread out
variance :: (RealFrac a, Fractional b) => [a] -> b
variance = mean . distancesFromMean where 
    distancesFromMean xs = map ((^2) . subtract (mean xs)) xs

stddev :: RealFloat a => [a] -> a
stddev = sqrt . variance

stdscore :: RealFloat a => a -> [a] -> a
stdscore x xs = (x - mean xs) / stddev xs

factorial :: Int -> Int
factorial n = product [1..n]

-- The number of ways to choose k elements from an n-element set.
binorm :: Int -> Int -> Int
binorm n k = product [k+1..n] `div` product [1..n-k]

-- The probability for k events of chance p to occur at the same time in an n-element set.
binormProb :: Floating a => Int -> Int -> a -> a
binormProb n k p = fromIntegral (binorm n k) * (p ** fromIntegral k) * ((1.0 - p) ** fromIntegral (n-k))

-- Probability density function
probDensity :: Floating a => a -> a
probDensity x = factor * exp (-0.5 * x * x)
                where factor = 1.0 / (sqrt (2.0 * pi))

-- Gaussian function
gaussian :: Floating a => a -> a -> a -> a
gaussian x mu sigma = 1.0 / sigma * probDensity ((x - mu) / sigma)

-- Measures how much two random variables change together.
-- covariance xs xs = variance xs
covariance :: RealFrac a => [a] -> [a] -> a
covariance xs ys = mean $ map distance (zip xs ys)
                   where xmu = mean xs
                         ymu = mean ys
                         distance (x, y) = (x-xmu)*(y-ymu)

-- Pearson's product-moment coefficient r                       
correlation xs ys = covariance xs ys / (stddev xs * stddev ys)
          
-- Gets the slope b of the linear regression line through the data.
--  (sum $ distancesFromMean2 xs ys) / (sum $ distancesFromMean ys)
slope xs ys = (covariance xs ys) / variance xs

-- Gets the intercept a of the linear regression line through the data.
intercept xs ys = mean ys - (b * mean xs) 
                  where b = slope xs ys

-- Gets the size of the confidence interval using -- the t-Distribution value [tfactor].
confinv xs tfactor = tfactor * (stddev xs / (sqrt (genericLength xs)))
