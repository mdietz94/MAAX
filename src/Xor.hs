module Xor where

import Data.List (foldl1')

xor :: Int -> Int -> Int
xor 1 0 = 1
xor 0 1 = 1
xor _ _ = 0

inputs :: [[Int]]
inputs = [[0,0],[0,1],[1,0],[1,1]]

outputs :: [Int]
outputs = map (foldl1' xor) inputs


{- To compute fitness for XOR, the distance of the output from the correct 
- answer is summed for all four input patterns. The result of this error
- is subtracted from four so that higher fitness reflect better
- network structure. The result is squared to give proportionally more
- fitness the closer the network is to a solution
-}
fitness :: [Int] -> Float
fitness xs = (fromIntegral (4 - sum (fmap (-) outputs xs))) ^ 2
