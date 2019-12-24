module Gondola where

import Data.List(groupBy)

groupPeopleForGondolaRide :: Int -> [Int]-> [[Int]]
groupPeopleForGondolaRide n input =
  let (a,b) = foldl (foldHelper n) ([],[]) (partitionBasedOnCapacity n input)
    in reverse (a:b)


foldHelper :: Int -> ([Int],[[Int]]) -> Int -> ([Int], [[Int]])
foldHelper n (acc , xxs) i | ((sum acc) + i <= n) = (i : acc , xxs)
foldHelper n ( acc , xxs) i | ((sum acc) + i > n) = (i : [] ,(reverse acc) : xxs)

partitionBasedOnCapacity :: Int -> [Int] -> [Int]
partitionBasedOnCapacity n input = concatMap (concatMapForPartitionBasedOnCapacity n []) input

concatMapForPartitionBasedOnCapacity :: Int -> [Int] -> Int -> [Int]
concatMapForPartitionBasedOnCapacity n acc input | input <= n = input : acc
concatMapForPartitionBasedOnCapacity n acc input =
  let divisor = div input n
      remainder = mod input n
   in if (remainder == 0) then ((replicate divisor n) ++ acc) else remainder : ((replicate divisor n)++ acc)

