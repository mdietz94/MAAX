{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module NeuralNetwork where

import Emulator
import System.Random
import Data.List
import Data.Maybe
import Control.Lens
import Control.Arrow ((&&&))
import Debug.Trace


data Config = Config { _numInputs            :: Int
                     , _numOutputs           :: Int
                     , _populationSize       :: Int
                     , _speciesMaxSize       :: Int
                     , _stagnationMax        :: Int
                     , _speciationThreshold  :: Float
                     , _weightedVsTopology   :: Float
                     , _crossoverChance      :: Float
                     , _smallScale           :: Float
                     , _largeScale           :: Float
                     , _maxLinkLength        :: Int
                     , _sigmoidFunction      :: Float -> Float
                     }
makeClassy ''Config

-- sigmoid step function
sigmoid :: Float -> Float
sigmoid x = 2.0 / (1.0 + exp (-4.9 * x)) - 1.0
sigmoidXor :: Float -> Float
sigmoidXor x = exp x / (1 + exp x)
sigmoidXor2 :: Float -> Float
sigmoidXor2 x = 1 / (1 + exp (0 - x))

xorConfig = Config { _numInputs = 2
                   , _numOutputs = 1
                   , _populationSize = 100
                   , _speciesMaxSize = 20
                   , _stagnationMax = 15
                   , _speciationThreshold = 3.0    -- this was 4.0 for DPLV (HARD Problem)
                   , _weightedVsTopology = 0.4
                   , _crossoverChance = 0.7
                   , _smallScale = 0.2
                   , _largeScale = 2.0
                   , _maxLinkLength = 10
                   , _sigmoidFunction = sigmoidXor
                   }

-- TODO:
--
-- add the ability to add biases to the netork
-- population size changes
-- sometimes networks have no connections to output node or no
-- connections from input node
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
                     } deriving (Eq)
makeClassy ''Genome

instance Show Genome where
    show (Genome n gs) = "Genome[" ++ show n ++ "]{" ++ 
                         concat (map (\g -> "\n" ++ show g) gs) ++ "\n}"

instance Show Gene where
    show (Gene inp out wt e i) 
      | e = "[x]" ++ show i ++ ": " ++ show inp ++ " --> " ++ show wt ++ " --> " ++ show out
      | otherwise = "[ ]" ++ show i

getInnovation genome inn = fromJust . find (\x -> x^.innovation == inn) $ genome^.genes



type Population = [Species]
type Species = ( Int                 --stagnation
               , Float               --max fitness
               , Float               --sum of fitnesses
               , (Float,Genome)      --representative genome
               , [(Float,Genome)])   --rest of genomes


--creates initial population
--random generator, intial size, num inputs, num outputs
--TODO add biases
initPopulation :: RandomGen g => g -> Config -> (Int,Population)
initPopulation rgen config = (length genes,[species]) where
  size = config^.populationSize
  in_max= config^.numInputs - 1
  out_max = in_max + config^.numOutputs
  species = (0,0.0,0.0,(0.0,genome),genomes)
  genomes = zip (repeat 0) (replicate size genome)
  genome = Genome (length genes) genes
  genes = zipWith3 (\g w i -> set weight w (set innovation i g))
                   g0s (randomRs (0,1) rgen) [0..]
  g0s = [ Gene inN outN 0 True 0 | inN <- [0 .. in_max] 
                                 , outN <- [in_max + 1 .. out_max] ]
  


--random generator, function from genome to a fitness, population, max
--number of generations to run
--run calculates the fitness of each genome in the population, then removes
--genomes from the population if their species has stagnated or if their
--species exceed the max size and they are least fit of their species, then
--produces the next generation of genomes and recurses with it
run :: RandomGen g => g -> Config -> (Genome -> Float) -> Int -> Population -> Int -> Population
run _ _ _ _ p0 0 = p0
run gen config fitnessFunc gInnov p0 n = run (snd $ next gen) config fitnessFunc gInnov' p3 (n - 1)
  where
    p2l = length p2
    p1 = map (evalSpecies fitnessFunc) p0
    p1sorted = map (\(a,b,c,d,gs) -> (a,b,c,d,sortBy (\(a,_) (b,_) -> compare a b) gs)) p1
    p2 = cull (config ^. speciesMaxSize) (config ^. stagnationMax) p1sorted
    (gInnov',p3) = reproduce gen config gInnov p2



--calculates the fitness of each genome in species
evalSpecies :: (Genome -> Float) -> Species -> Species
evalSpecies fitnessFunc s@(i0,max_f0,sum_f0,g0,gs0) = (i',max_f,sum_f,g0,gs') where
  i' | max_f0 < max_f = 0 | otherwise = i0 + 1 --update stagnation counter
  ((sum_f,max_f),gs') = mapAccumL go (0,0) gs0 --get sum and max fitness of species
  n = fromIntegral $ length gs0 --number of genomes in species
  go :: (Float,Float) -> (Float,Genome) -> ((Float,Float),(Float,Genome))
  go (acc,old_max) (_,g) = ((acc + f',new_max),(f',g)) where
    f = fitnessFunc g
    f' = f / n
    new_max = max old_max f'


--produces the next generation of genomes
--assumes the list of genomes is sort in ascending order by fitness
reproduce :: RandomGen g => g -> Config -> Int -> Population -> (Int,Population)
reproduce gen config gInnov0 population = (gInnov'',population') where
  p_sum_f = foldl' (\a (_,_,f,_,_) -> a + f) 0.0 population --count population sum fitness
  prev_species = map (\(i0,_,_,g,gs) -> 
                         let (g',_) = randomElem gen gs 
                         in (i0,0,0,g',[])) 
                     population --pick random genome from prev generation
  genomes = concat gss
  (gInnov'',gss) = mapAccumR (\gInnov (i0,max_f0,sum_f0,g0,gs0) -> 
                                let num_offspring = round $ sum_f0 / p_sum_f * p_size
                                    len = length gs0
                                    (gs0',stud) | len > 5 = splitAt (len - 1) gs0 --don't breed the stud
                                                | otherwise = (gs0,[])
                                    (gInnov',gs',_) = breedSpecies config gInnov num_offspring (map snd gs0') rs
                                in (gInnov',map snd stud ++ gs')) 
                             gInnov0 population
  population' = cullEmpty $ speciefy wghtVsTop specThresh prev_species genomes
  rs = randomRs (0,0.999999999) gen
  wghtVsTop = config^.weightedVsTopology
  specThresh = config^.speciationThreshold
  p_size = fromIntegral $ config^.populationSize

--divides a list of genomes into a list os species
--requires a list of representative species from the last generation
speciefy :: Float -> Float -> [Species] -> [Genome] -> [Species]
speciefy _ _ species [] = species
speciefy wghtVsTop specThresh species (g:gs)
  | isNothing matchIx = speciefy wghtVsTop specThresh ((0,0,0,(0,g),[(0,g)]):species) gs
  | otherwise = speciefy wghtVsTop specThresh species' gs 
  where
    matchIx = findSpecies wghtVsTop specThresh g species
    i = fromJust matchIx
    (si,sm,ss,sg,sgs) = species !! i
    species' = take i species ++ (si,sm,ss,sg,(0,g):sgs) : drop (i + 1) species
  
findSpecies :: Float -> Float -> Genome -> [Species] -> Maybe Int
findSpecies wghtVsTop specThresh g = findIndex (\(_,_,_,(_,repG),_) -> geneticDifference wghtVsTop repG g < specThresh)


--removes species whos fitness has stagnated and removes
--the least fit genomes from each species
--assumes genomes are sorted in ascending order by fitness
cull :: Int -> Int -> Population -> Population
cull maxSize maxStag = map (cullSpecies maxSize) . filter (\(i,_,_,_,_) -> i < maxStag)


--removes species which no longer have any members
cullEmpty :: [Species] -> [Species]
cullEmpty = filter (\(_,_,_,_,gs) -> not $ null gs)

-- is this done right ? the formula from the paper is
-- delta = c1*E/N + c2*D/N + c3*W_bar 
-- where E is number of excess genes, D is number of disjoint genes,
-- N is number of genes in larger genome, and W_bar is the average weight
-- differences of matching genes
--  union \\ intersection does the job, could probably be made efficienter
geneticDifference :: Float -> Genome -> Genome -> Float
geneticDifference wghtVsTop g1 g2 = excess_disjoint / num + wghtVsTop * (diff / fromIntegral (length genesBoth))
    where
        excess_disjoint = fromIntegral . length $ (union genes1Inn genes2Inn) \\ (intersect genes1Inn genes2Inn)
        genes1Inn = map (^.innovation) . filter (^.enabled) $ g1^.genes
        genes2Inn = map (^.innovation) . filter (^.enabled) $ g2^.genes
        genesBoth = map (getInnovation g1 &&& getInnovation g2) $ intersect genes1Inn genes2Inn
        diff = sum $ map (\(a,b) -> abs $ a^.weight - b^.weight) genesBoth
        num = fromIntegral $ max (g1^.genes.to length) (g2^.genes.to length)

breedSpecies :: Config -> Int -> Int -> [Genome] -> [Float] -> (Int,[Genome],[Float])
breedSpecies _ gInnov 0 species rs = (gInnov,[],rs)
breedSpecies config gInnov n species rs = (gInnov'',newChild : otherChildren, rs'')
    where
        (gInnov',newChild,rs') = breedChild config gInnov species rs
        (gInnov'',otherChildren, rs'') = breedSpecies config gInnov' (n-1) species rs'

-- breedChild requires that only genomes IN THE SAME SPECIES are passed
breedChild :: Config -> Int -> [Genome] -> [Float] -> (Int,Genome,[Float])
breedChild _ _ [] _ = error "empty gs"
breedChild config gInnov gs (r:(r1:(r2:rs))) = (gInnov',monster,rs'')
  where dad = getElementR gs r1
        mom = getElementR gs r2
        (child,rs') | r < 0.7 = crossover dad mom rs
                    | otherwise = (getElementR gs r1, r2:rs)
        (gInnov',monster,rs'') = mutate config gInnov child rs'
            

cullSpecies :: Int -> Species-> Species
cullSpecies numberToLeave (i,m,s,g,gs) = (i,m,s,g,gs')
  where sorted = sortBy (\(a,_) (b,_) -> compare a b) gs
        gs' = take numberToLeave sorted


--a more general evalute genome?
evaluateGenome :: Config -> [Float] -> Genome -> [Float]
evaluateGenome config inputs (Genome maxNode genes) = outs
    where
        maxLL = config^.maxLinkLength
        numIn = config^.numInputs
        numOut = config^.numOutputs
        sigmoid = config^.sigmoidFunction
        outs = map (evaluateNode maxLL) [numIn..numIn + numOut - 1]
        evaluateNode :: Int -> Int -> Float
        evaluateNode links n
          | links == 0 = 0.0
          | n < numIn = inputs !! n
          | otherwise = sigmoid . sum . map evaluateGene . filter isMyGene $ genes
            where
                evaluateGene :: Gene -> Float
                evaluateGene g = g^.weight * evaluateNode (links-1) (g^.input)
                isMyGene :: Gene -> Bool
                isMyGene g = g^.enabled && g^.output == n

addNode :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
addNode gInnov g0@(Genome numnodes genes) (r:rs) 
  | null enabledGenes = (gInnov,g0,r:rs)
  | otherwise = (gInnov+2,Genome (numnodes+1) genes', rs)
    where
        enabledGenes = filter (^.enabled) genes
        rGene = getElementR enabledGenes r
        newGene1 = set innovation gInnov . set output numnodes $ rGene
        newGene2 = set innovation (gInnov+1) . set weight  1.0 .  set input numnodes $ rGene
        genes' = [newGene1, newGene2, enabled .~ False $ rGene] ++ delete rGene genes

addLink :: Config -> Int -> Genome -> [Float] -> (Int,Genome,[Float])
addLink config gInnov (Genome nodes genes) (r:(r1:rs)) 
  | null disjointPairs = (gInnov,Genome nodes genes, r:r1:rs)
  | otherwise = (gInnov+1,Genome nodes (newGene : genes), rs)
    where
        inputMax = (config^.numInputs) - 1
        outputMax = inputMax + (config^.numOutputs)
        allPairs = [ (x,y) | x <- [0 .. inputMax] ++ [outputMax + 1 .. nodes]
                           , y <- [inputMax + 1 .. nodes] ]
        gPairs = map (\g -> (g^.input,g^.output)) genes
        disjointPairs = allPairs \\ gPairs
        (inp,out) = getElementR disjointPairs r
        newGene = Gene inp out (r1 * 4.0 - 2.0) True gInnov

disableGene :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
disableGene gInnov g0@(Genome nodes genes) (r:rs) 
  | null enabledGenes = (gInnov,g0,r:rs)
  | otherwise = (gInnov, Genome nodes genes', rs)
    where
        enabledGenes = filter (^.enabled) genes
        rGene = getElementR enabledGenes r
        genes' = (enabled .~ False $ rGene) : delete rGene genes

enableGene :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
enableGene gInnov g0@(Genome nodes genes) (r:rs) 
  | null disabled = (gInnov, g0,r:rs)
  | otherwise = (gInnov, Genome nodes genes', rs)
    where
        disabled = filter (not . (^.enabled)) genes
        rGene = getElementR disabled r
        genes' = (enabled .~ True $ rGene) : delete rGene genes

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

-- should mess with these rates of mutation...
mutate :: Config -> Int -> Genome -> [Float] -> (Int,Genome,[Float])
mutate config gInnov genome (r:rs)
  | r < 0.1 = uncurry3 (mutate config) $ addLink config gInnov genome rs
  | r < 0.2 = uncurry3 (mutate config) $  addNode gInnov genome rs
  | r < 0.3 = uncurry3 (mutate config) $ disableGene gInnov genome rs
  | r < 0.4 = uncurry3 (mutate config) $ enableGene gInnov genome rs
  | otherwise = (gInnov, perturbWeights genome rs, drop (genome^.genes.to length) rs)
    where
        perturbWeights :: Genome -> [Float] -> Genome
        perturbWeights genome rs = genes %~ map (uncurry perturb) . zip rs $ genome
        perturb :: Float -> Gene -> Gene
        perturb r = if smallChange then weight +~ r * 0.2 - 0.1 / 2.0 else weight .~ r * 4.0 - 2.0
            where
                smallChange = floor (r * 100.0) `mod` 10 > 0


-- genome1 MUST be fitter than genome2!
-- breeds 2 genomes together
crossover :: Genome -> Genome -> [Float] -> (Genome, [Float])
crossover genome1 genome2 rs = (genes .~ genes2 ++ genes1 $ genome1, drop (genome2^.genes.to length) rs)
    where
        innovationNums = map (^.innovation) $ genome1^.genes
        genes2 = map snd . filter (\(r,g)->  r < 0.5 && g^.innovation `elem` innovationNums) . zip rs $ genome2^.genes
        genes1 = deleteFirstsBy (\a b -> a^.innovation == b^.innovation) (genome1^.genes) genes2


-- just gets an element, convenient for using randoms
getElementR :: [a] -> Float -> a
getElementR xs r = xs !! (floor ( r * fromIntegral (length xs)))

maxFittestSpecies :: Population -> Species
maxFittestSpecies = maximumBy (\(_,a,_,_,_) (_,b,_,_,_) -> compare a b)

maxFittestGenome :: Species -> Genome
maxFittestGenome (_,_,_,_,gs) = snd (maximumBy (\(f0,_) (f1,_) -> compare f0 f1) gs)

fittestGenome :: Population -> Genome
fittestGenome [] = error "empty population"
fittestGenome p = maxFittestGenome . maxFittestSpecies $ p

randomElem :: RandomGen g => g -> [a] -> (a,g)
randomElem gen xs 
  | i < 0 || i >= length xs = error "random elem"
  | otherwise = (xs !! i,gen') 
  where (i,gen') = randomR (0,length xs - 1) gen


mapT :: (a -> b) -> (a,a) -> (b,b)
mapT f (a,b) = (f a,f b)

lengthNum :: Num b => [a] -> b
lengthNum = fromIntegral . length


--XOR code for testing neural network
xor :: Float -> Float -> Float
xor 1 0 = 1
xor 0 1 = 1
xor _ _ = 0

inputs :: [[Float]]
inputs = [[0,0],[0,1],[1,0],[1,1]]

outputs :: [Float]
outputs = map (foldl1' xor) inputs

{- To compute fitness for XOR, the distance of the output from the correct 
- answer is summed for all four input patterns. The result of this error
- is subtracted from four so that higher fitness reflect better
- network structure. The result is squared to give proportionally more
- fitness the closer the network is to a solution
-}
fitnessXor :: Config -> Genome -> Float
fitnessXor config g = let genome_outs = concat $ map (flip (evaluateGenome config) g) inputs
                      in (4 - sum (map abs (zipWith (-) outputs genome_outs))) ^ 2
