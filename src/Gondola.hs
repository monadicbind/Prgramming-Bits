module Gondola where

import Data.List(groupBy)


groupPeopleForGondolaRide :: Int -> [Int]-> [[Int]]
groupPeopleForGondolaRide n input =
  let (a,b) = foldr (foldHelper n) ([],[]) (partitionBasedOnCapacity n input)
    in reverse (a:b)


foldHelper :: Int -> Int -> ([Int],[[Int]]) -> ([Int], [[Int]])
foldHelper n i (acc , xxs) | sum acc + i <= n = (i : acc , xxs)
foldHelper n i (acc , xxs) | sum acc + i > n = ([i] ,reverse acc : xxs)

partitionBasedOnCapacity :: Int -> [Int] -> [Int]
partitionBasedOnCapacity n input =  concat (rearrangeRemainder n (map (concatMapForPartitionBasedOnCapacity n []) input))

rearrangeRemainder :: Int ->  [[Int]] -> [[Int]]
rearrangeRemainder n =  foldr (rearrangeRemainderFold n) [[]]

rearrangeRemainderFold :: Int -> [Int] -> [[Int]] -> [[Int]]
rearrangeRemainderFold _ init []= [init]
rearrangeRemainderFold n curr@(y:ys) acc@(x:xs)| abs (subtract n (sum x)) >= y = curr:acc
rearrangeRemainderFold n (y :ys) acc = (ys ++[y]) : acc

concatMapForPartitionBasedOnCapacity :: Int -> [Int] -> Int -> [Int]
concatMapForPartitionBasedOnCapacity n acc input | input <= n = input : acc
concatMapForPartitionBasedOnCapacity n acc input =
  let divisor = div input n
      remainder = mod input n
      replc = replicate divisor n
    in if (remainder == 0) then (replc ++ acc) else (remainder:(replc++ acc))

