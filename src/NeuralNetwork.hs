{-# LANGUAGE TemplateHaskell #-}

module NeuralNetwork where

import Emulator
import System.Random
import Data.List
import Data.Maybe
import Control.Lens
import Control.Arrow ((&&&))

populationSize = 200

numInputs = 8
numOutputs = 6

isInput = (<numInputs)
isOutput n = not (isInput n) && n < numInputs+numOutputs
isHidden n = not (isInput n) && not (isOutput n)

maxStagnation = 15 --generations a genome is allowed to survive without improving fitness

-- TODO:
-- if a species does not improve fitness after 15 generations
-- we need to eliminate it
--    todo this we keep track of a counter with each genome
--    if fitness improves we reset counter of genome to 0
--    otherwise we increment the counter
--    when we cull the population, we check if the counter exceeds
--    the maxStagnation limit, if so we cull the genome
-- The best genome of each species with at least 5 genomes
-- should be copied to the next generation unmodified
--
-- Mutation rates:
-- 80% of weight mutation
--     90% being perturbed
--     10% being assigned new random value
-- 75% inherited gene disabled if disabled in EITHER parent
-- 25% no crossover -- DONE
-- 0.1% interspecies mating rate
-- 3% chance of adding new node
-- 5% chance of adding new link

data Gene = Gene { _input      :: Int
                 , _output     :: Int
                 , _weight     :: Float
                 , _enabled    :: Bool
                 , _innovation :: Int } deriving (Eq)
makeClassy ''Gene

data Genome = Genome { _numnodes :: Int
                     , _genes :: [Gene]
                     , _fitness :: Float
                     , _stagnation :: Int }
makeClassy ''Genome

type Population = [Genome]

getInnovation genome inn = fromJust . find (\x -> x^.innovation == inn) $ genome^.genes

speciationThreshold = 3.0 :: Float -- this was 4.0 for DPLV (HARD Problem)

weightedVsTopology = 0.4 -- this was 3.0 for DPLV (HARD problem)

-- is this done right ? the formula from the paper is
-- delta = c1*E/N + c2*D/N + c3*W_bar 
-- where E is number of excess genes, D is number of disjoint genes,
-- N is number of genes in larger genome, and W_bar is the average weight
-- differences of matching genes
-- previously  if
--      g1 = [1,2,3,4,5,8]
--      g2 = [1,2,3,4,5,6,7,9,10]
--      then g1 \\ g2 --> [8]
--      but we want disjoint + excess to be [6,7,8,9,10]
--      so union \\ intersection does the job, could probably be made more
--      effiecient
geneticDifference :: Genome -> Genome -> Float
geneticDifference g1 g2 = excess_disjoint / num + weightedVsTopology * (diff / fromIntegral (length genesBoth))
    where
        excess_disjoint = fromIntegral . length $ (union genes1Inn genes2Inn) \\ (intersect genes1Inn genes2Inn)
        genes1Inn = map (^.innovation) . filter (^.enabled) $ g1^.genes
        genes2Inn = map (^.innovation) . filter (^.enabled) $ g2^.genes
        genesBoth = map (getInnovation g1 &&& getInnovation g2) $ intersect genes1Inn genes2Inn
        diff = sum $ map (\(a,b) -> abs $ a^.weight - b^.weight) genesBoth
        num = fromIntegral $ max (g1^.genes.to length) (g2^.genes.to length)

breedSpecies :: Int -> [Genome] -> [Float] -> ([Genome],[Float])
breedSpecies 0 species rs = ([],rs)
breedSpecies n species rs = (newChild : otherChildren, rs'')
    where
        (otherChildren, rs'') = breedSpecies (n-1) species rs'
        (newChild,rs') = breedChild species rs

-- breedChild requires that only genomes IN THE SAME SPECIES are passed
crossoverChance = 0.7
breedChild :: [Genome] -> [Float] -> (Genome,[Float])
breedChild gs (r:(r1:(r2:rs)))
  | r < 0.7 = crossover (getElementR gs r1) (getElementR gs r2) rs
  | otherwise = (getElementR gs r1, r2:rs)

-- cullSpecies takes the number to keep per species
-- along with the population (a list of each genome tupled with
-- its fitness and its fitness x generations ago
-- so this should be called after each generation is done
-- running
cullSpecies :: Int -> [Genome] -> [Genome]
cullSpecies numberToLeave population = concatMap (take numberToLeave) speciesSorted
    where
        population' = filter (\g -> g^.stagnation >= maxStagnation) population
        species :: [[Genome]]
        species = groupBy (\a b -> geneticDifference a b < speciationThreshold) population'
        speciesSorted :: [[Genome]]
        speciesSorted = map (sortBy (\a b -> compare (a^.fitness) (b^.fitness))) species

-- the sum of the adjustedFitness
-- of a species determines
-- the number of offspring they will
-- have in the next generation
adjustedFitness :: Genome -> Population -> Float
adjustedFitness genome population = genome^.fitness / modifier
    where
        modifier = sum $ map (sharing . geneticDifference genome) population
        sharing x = if x < speciationThreshold then 1.0 else 0.0

-- greater than 0.5 and we press the button
-- output neurons must be in same order as
-- fromListJ expects
-- though obviously its random start so this
-- will always evolve to work
maxLinkLength = 40 -- the max number to backjump, so we don't get stuck in loops
evaluateGenome :: Genome -> [Float] -> Joystick
evaluateGenome (Genome maxNode genes _ _) inputs = fromListJ (map (>0.5) outs)
    where
        outs = map (evaluateNode maxLinkLength) [numInputs..numInputs+numOutputs]
        evaluateNode :: Int -> Int -> Float
        evaluateNode links n
          | links == 0 = 0.0
          | isInput n = inputs !! n
          | otherwise = sigmoid . sum . map evaluateGene . filter isMyGene $ genes
            where
                evaluateGene :: Gene -> Float
                evaluateGene g = g^.weight * evaluateNode (links-1) (g^.input)
                isMyGene :: Gene -> Bool
                isMyGene g = g^.enabled && g^.output == n

addNode :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
addNode gInnov (Genome numnodes genes _ _) (r:rs) = (gInnov+2,Genome (numnodes+1) genes' (error "fitness") 0, rs)
    where
        rGene = getElementR (filter (^.enabled) genes) r
        newGene1 = set innovation gInnov . set output numnodes $ rGene
        newGene2 = set innovation (gInnov+1) . set weight  1.0 .  set input numnodes $ rGene
        genes' = [newGene1, newGene2, enabled .~ False $ rGene] ++ delete rGene genes

addLink :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
addLink gInnov (Genome nodes genes _ _) (r:(r1:rs)) = (gInnov+1,Genome nodes (newGene : genes) (error "fitness") 0, rs)
    where
        allPairs = [ (x,y) | x <- [0..nodes], y <- [0..nodes] ]
        gPairs = map (\g -> (g^.input,g^.output)) genes
        disjointPairs = allPairs \\ gPairs
        (inp,out) = getElementR disjointPairs r
        newGene = Gene inp out (r1 * 4.0 - 2.0) True gInnov

disableGene :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
disableGene gInnov (Genome nodes genes fitness stagnation) (r:rs) = (gInnov, Genome nodes genes' fitness stagnation, rs)
    where
        rGene = getElementR (filter (^.enabled) genes) r
        genes' = (enabled .~ False $ rGene) : delete rGene genes

enableGene :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
enableGene gInnov (Genome nodes genes fitness stagnation) (r:rs) = (gInnov, Genome nodes genes' fitness stagnation, rs)
    where
        rGene = getElementR (filter (not . (^.enabled)) genes) r
        genes' = (enabled .~ True $ rGene) : delete rGene genes

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

-- should mess with these rates of mutation...
mutate :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
mutate gInnov genome (r:rs)
  | r < 0.1 = uncurry3 mutate $ addLink gInnov genome rs
  | r < 0.15 = uncurry3 mutate $ addNode gInnov genome rs
  | r < 0.45 = uncurry3 mutate $ disableGene gInnov genome rs
  | r < 0.6 = uncurry3 mutate $ enableGene gInnov genome rs
  | otherwise = (gInnov, perturbWeights genome rs, drop (genome^.genes.to length) rs)
    where
        perturbWeights :: Genome -> [Float] -> Genome
        perturbWeights genome rs = genes %~ map (uncurry perturb) . zip rs $ genome
        perturb :: Float -> Gene -> Gene
        perturb r = if smallChange then weight +~ r * 0.2 - 0.1 / 2.0 else weight .~ r * 4.0 - 2.0
            where
                smallChange = floor (r * 100.0) `mod` 10 > 0

-- 90% chance to make a small change
-- 10% chance to reset completely
smallScale = 0.2
largeScale = 2.0

-- genome1 MUST be fitter than genome2!
-- breeds 2 genomes together
crossover :: Genome -> Genome -> [Float] -> (Genome, [Float])
crossover genome1 genome2 rs = (genes .~ genes2 ++ genes1 $ genome1, drop (genome2^.genes.to length) rs)
    where
        innovationNums = map (^.innovation) $ genome1^.genes
        genes2 = map snd . filter (\(r,g)->  r < 0.5 && g^.innovation `elem` innovationNums) . zip rs $ genome2^.genes
        genes1 = deleteFirstsBy (\a b -> a^.innovation == b^.innovation) (genome1^.genes) genes2

-- sigmoid step function
sigmoid :: Float -> Float
sigmoid x = 2.0 / (1.0 + exp (-4.9 * x)) - 1.0

-- just gets an element, convenient for using randoms
getElementR :: [a] -> Float -> a
getElementR xs r = xs !! floor ( r * fromIntegral (length xs))

