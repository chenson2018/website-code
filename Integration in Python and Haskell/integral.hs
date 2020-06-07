#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

linspace :: Double -> Double -> Int -> [Double]
linspace a b n =
   map (\ i -> a + (fromIntegral i) * inc) [(0::Int) .. (n - 1)]
  where
   inc = (b - a) / (fromIntegral (n - 1))

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = 
  x : if p x then takeWhileInclusive p xs
             else []

darboux :: (Double -> Double) -> Double -> Double -> Double -> Int -> Int -> [(Double, Double, Int)]
darboux f a b epsilon precision increment = res 
 where
  n = map (\x-> increment*x) [1..]
  
  partitions = map (\n -> linspace a b (n+1)) n
  delta = map (\xs -> (xs !! 1) - (xs !! 0)) partitions
  
  pairs = map (\xs -> zip xs (tail xs)) partitions
  options = (map.map) (\(start, end) -> linspace start end precision) pairs
  values  = (map.map.map) (\x -> f x) options
  
  upper = map sum $ (map.map) maximum values
  lower = map sum $ (map.map) minimum values
  
  upper_sum = zipWith (*) upper delta
  lower_sum = zipWith (*) lower delta
  
  s = zip3 lower_sum upper_sum n
  res = takeWhileInclusive (\(low, high, n) -> (high - low) > epsilon) s

main = do
    let test = darboux (\x -> x**2+2*x) 3 4.5 0.001 5 500
    print (last test)
    