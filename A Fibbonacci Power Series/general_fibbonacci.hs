#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import Math.NumberTheory.Powers.Squares
import Data.Ratio
import Data.Sort

--takes in g_0 and g_1
--gives the series starting from g_0
gen_fib :: Int -> Int -> [Int]
gen_fib a b = seq where
              seq = a : b : zipWith (+) seq (tail seq)

--takes in g_0 and g_1 and reurns the discriminant function
discriminant_check :: Int -> Int -> (Int -> Int)
discriminant_check a b = f where
                         f s = s^2 + 2*s*b + b^2 + 4*s^2 + 4*a*s

generator_func :: Ratio Integer -> Ratio Integer -> (Ratio Integer -> Ratio Integer)
generator_func a b = f where
               f x = (a - a*x + b*x)/(1-x-x^2) - a

fib = gen_fib 0 1
gen = gen_fib 3 1

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) = if x < y
                        then x:(merge xs (y:ys))
                        else y:(merge (x:xs) ys)
merge [] xs = xs
merge xs [] = xs

main = do
    {-Problem 137-}
    --use this to get a partial answer
    --we can only take so many before computation is too slow
    let discriminant_fib = discriminant_check 0 1
    let partial_nuggets = take 7 $ filter (\x -> isSquare (discriminant_fib x)) [1..]
    
    --this uses the recursive formula for the Fibbonacci power series
    let fib_nugget = take 15 $ 
                     filter (>0) $ 
                     zipWith (*) [j | (i, j) <- zip [1..] fib, even i] 
                                 [j | (i, j) <- zip [1..] fib, odd i]
    
    --we could also use the generating function directly
    let fib_generator = generator_func 0 1
    let fib_nugget_2 = take 15 $
                       filter (>0) $
                       map (fib_generator) $ 
                       zipWith (%) (map (toInteger) [j | (i, j) <- zip [2..] fib, even i]) 
                                   (map (toInteger) [j | (i, j) <- zip [2..] fib, odd i])

    {-Problem 140-}
    --again, we can get some nuggets as above
    let discriminant_gen = discriminant_check 3 1
    let partial_nuggets_gen = take 15 $ filter (\x -> isSquare (discriminant_gen x)) [1..]
    
    --use the generating function with this recursion
    let gen_func = generator_func 3 1
    
    --nuggets generated recursively from the new sequence
    let g_nug = map (gen_func) $
                zipWith (%) (filter (>0) $ map (toInteger) [j | (i, j) <- zip [1..] (gen_fib 2 5), odd i]) 
                            (filter (>0) $ map (toInteger) [j | (i, j) <- zip [1..] (gen_fib 2 5), even i])
    
    --nuggets generated recursively from the base fibbonacci numbers
    let f_nug = map (gen_func) $
                [j | (i, j) <- zip [1..] (zipWith (%) (map (toInteger) (gen_fib 1 2)) 
                                                      (map (toInteger)(gen_fib 2 3))), odd i]
                
    let nug = take 30 $ merge g_nug f_nug
    
    --prints the first 15 nuggets for problem 137,
    --and the first 30 for problem 140
    mapM_ print [map (numerator) fib_nugget_2, 
                 [], 
                 map (numerator) nug]