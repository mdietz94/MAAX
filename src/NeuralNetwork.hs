{-# LANGUAGE TemplateHaskell #-}

module NeuralNetwork where

import Emulator
import System.Random
import Data.List
import Data.Map (Map)
import Data.Maybe
import Control.Lens
import qualified Data.Map as Map

numInputs = 8
numOutputs = 6

isInput = (<numInputs)
isOutput n = not (isInput n) && n < numInputs+numOutputs
isHidden n = not (isInput n) && not (isOutput n)

-- we need to check for loops here...
-- other than that this seems to be coming along real nicely

--- so slight mistake is that the nodes are actually genome independent... methinks...
-- or we don't actually need to store them, since they don't actually store info. other than
-- hidden/output/input
data Gene = Gene { _input      :: Int
                 , _output     :: Int
                 , _weight     :: Float
                 , _enabled    :: Bool
                 , _innovation :: Int } deriving (Eq)
makeClassy ''Gene

data Genome = Genome { _numnodes :: Int
                     , _genes :: [Gene] }
makeClassy ''Genome

-- greater than 0.5 and we press the button
-- output neurons must be in same order as
-- fromListJ expects
-- though obviously its random start so this
-- will always evolve to work
evaluateGenome :: Genome -> [Float] -> Joystick
evaluateGenome (Genome maxNode genes) inputs = fromListJ (map (>0.5) outs)
    where
        outs = map evaluateNode [numInputs..numInputs+numOutputs]
        evaluateNode :: Int -> Float
        evaluateNode n
          | isInput n = inputs !! n
          | otherwise = sigmoid . sum . map evaluateGene . filter isMyGene $ genes
            where
                evaluateGene :: Gene -> Float
                evaluateGene g = g^.weight * evaluateNode (g^.input)
                isMyGene :: Gene -> Bool
                isMyGene g = g^.enabled && g^.output == n

addNode :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
addNode gInnov (Genome numnodes genes) (r:rs) = (gInnov+2,Genome (numnodes+1) genes', rs)
    where
        rGene = getElementR (filter (^.enabled) genes) r
        newGene1 = set innovation gInnov . set output numnodes $ rGene
        newGene2 = set innovation (gInnov+1) . set weight  1.0 .  set input numnodes $ rGene
        genes' = [newGene1, newGene2, enabled .~ False $ rGene] ++ delete rGene genes

addLink :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
addLink gInnov (Genome nodes genes) (r:(r1:rs)) = (gInnov+1,Genome nodes (newGene : genes), rs)
    where
        allPairs = [ (x,y) | x <- [0..nodes], y <- [0..nodes] ]
        gPairs = map (\g -> (g^.input,g^.output)) genes
        disjointPairs = allPairs \\ gPairs
        (inp,out) = getElementR disjointPairs r
        newGene = Gene inp out (r1 * 4.0 - 2.0) True gInnov

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

-- should mess with these rates of mutation...
mutate :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
mutate gInnov genome (r:rs)
  | r < 0.2 = uncurry3 mutate $ addLink gInnov genome rs
  | r < 0.5 = uncurry3 mutate $ addNode gInnov genome rs
  | otherwise = (gInnov, perturbWeights genome rs, drop (genome^.genes.to length) rs)
    where
        perturbWeights :: Genome -> [Float] -> Genome
        perturbWeights genome rs = genes %~ map (uncurry perturb) . zip rs $ genome
        perturb :: Float -> Gene -> Gene
        perturb r = weight +~ r * scale - scale / 2.0
            where
                scale = if floor (r * 100.0) `mod` 10 > 0 then 0.2 else 2.0

-- 90% chance to make a small change
-- 10% chance to reset completely
smallScale = 0.2
largeScale = 2.0

-- genome1 MUST be fitter than genome2!
crossover :: Genome -> Genome -> [Float] -> (Genome, [Float])
crossover genome1 genome2 rs = (genes .~ genes2 ++ genes1 $ genome1, drop (genome2^.genes.to length) rs)
    where
        innovationNums = map (^.innovation) $ genome1^.genes
        genes2 = map snd . filter (\(r,g)->  r < 0.5 && g^.innovation `elem` innovationNums) . zip rs $ genome2^.genes
        genes1 = deleteFirstsBy (\a b -> a^.innovation == b^.innovation) (genome1^.genes) genes2

sigmoid :: Float -> Float
sigmoid x = 2.0 / (1.0 + exp (-4.9 * x)) - 1.0

getElementR :: [a] -> Float -> a
getElementR xs r = xs !! floor ( r * fromIntegral (length xs))
