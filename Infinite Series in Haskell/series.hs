#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

infin_series :: (Double -> Double) -> [Int] -> [Double]
infin_series func index = map (func . fromIntegral) index

basel :: Int -> Double
basel sum_terms = sum $ 
                  take sum_terms $
                  infin_series (\n -> 1/(n**2)) [1..]

main = do
    let approximation = basel 10000
    print approximation