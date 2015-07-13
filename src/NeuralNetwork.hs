
module NeuralNetwork where

import Types
import Emulator
import Data.List
import Data.Maybe
import Control.Lens
import Control.Arrow ((&&&))
import Debug.Trace
import Data.Binary



-- sigmoid step function
sigmoid :: Float -> Float
sigmoid x = 2.0 / (1.0 + exp (-4.9 * x)) - 1.0
sigmoidXor :: Float -> Float
sigmoidXor x = exp x / (1 + exp x)
sigmoidXor2 :: Float -> Float
sigmoidXor2 x = 1 / (1 + exp (negate x))

xorConfig = Config { _numInputs = 2
                   , _numOutputs = 1
                   , _populationSize = 150
                   , _speciesMaxSize = 30
                   , _stagnationMax = 15
                   , _speciationThreshold = 0.1    -- this was 4.0 for DPLV (HARD Problem)
                   , _weightedVsTopology = 0.4
                   , _crossoverChance = 0.75
                   , _smallScale = 0.2
                   , _largeScale = 2.0
                   , _maxLinkLength = 10
                   , _sigmoidFunction = sigmoid
                   }

marioConfig = Config { _numInputs = 169
                     , _numOutputs = 6
                     , _populationSize = 200
                     , _speciesMaxSize = 60
                     , _stagnationMax = 15
                     , _speciationThreshold = 3.0
                     , _weightedVsTopology = 0.6
                     , _crossoverChance = 0.75
                     , _smallScale = 0.2
                     , _largeScale = 2.0
                     , _maxLinkLength= 20
                     , _sigmoidFunction = sigmoid
                     }

-- Mutation rates:
-- 80% of weight mutation
--     90% being perturbed
--     10% being assigned new random value
-- 75% inherited gene disabled if disabled in EITHER parent
-- 25% no crossover -- DONE
-- 0.1% interspecies mating rate
-- 3% chance of adding new node
-- 5% chance of adding new link


getInnovation genome inn = fromJust . find (\x -> x^.innovation == inn) $ genome^.genes

type Population = [Species]
type Species = ( Int                 --stagnation
               , Float               --max fitness
               , Float               --sum of fitnesses
               , Genome              --representative genome
               , [Genome])           --rest of genomes

savePopulation :: String -> Population -> IO ()
savePopulation = encodeFile

loadPopulation :: String -> IO Population
loadPopulation = decodeFile

--creates initial population
--random generator, intial size, num inputs, num outputs
initPopulation :: [Float] -> Config -> (Int,Population)
initPopulation rgen config = (length genes,[species]) where
  size = config^.populationSize
  in_max= config^.numInputs - 1
  out_max = in_max + config^.numOutputs
  species = (0,0.0,0.0,repGenome,genomes)
  repGenome = Genome 0.0 (out_max + 1) genes
  genomes' = replicate size (\r -> Genome 0.0 (out_max + 1) (zipWith (set weight) r genes))
  genes = zipWith (set innovation) [0..] g0s
  g0s = [ Gene inN outN 1.0 True 0 | inN <- [0 .. in_max]
                                 , outN <- [in_max + 1 .. out_max] ]
  genomes = zipWith (\r f -> f r)  [ map ((\x -> x-2.0) . (*4.0)) rs | rs <- iterate (drop (length genes)) rgen ] genomes'


-- TODO:
--  we cull the population before reproduce. The paper implies to cull the
--  population AFTER reproduction. This might effect the adjusted fitness
--  of each species
--      tried this and didn't make difference - JR

--random generator, function from genome to a fitness, population, max
--number of generations to run
--run calculates the fitness of each genome in the population, then removes
--genomes from the population if their species has stagnated or if their
--species exceed the max size and they are least fit of their species, then
--produces the next generation of genomes and recurses with it
run :: [Float] -> Config -> (Genome -> Float) -> Int -> Population -> Int -> Population
run _ _ _ _ p0 0 = p0
run gen config fitnessFunc gInnov p0 n = trace t $ run gen' config fitnessFunc gInnov' p3 (n - 1)
  where
    t = intercalate ("\n" ++ replicate 50 '-' ++ "\n") $ map str [p1',p2,p3]
    str x = intercalate "\n" (map speciesInfo x)
    stagnant = zipWith (<=) (map (\(_,m,_,_,_) -> m) p1) (map (\(_,m,_,_,_) -> m) p0)
    p1' = zipWith (\a (s,m,fit,rep,gen) -> if a then (s+1,m,fit,rep,gen) else (0,m,fit,rep,gen)) stagnant p1
    p1 = map (evalSpecies fitnessFunc) p0
    p1sorted = map sortSpecies p1'
    p2 = cull (config ^. speciesMaxSize) (config ^. stagnationMax) p1sorted
    (gInnov',p3,gen') = reproduce gen config gInnov p2

--calculates the fitness of each genome in species
evalSpecies :: (Genome -> Float) -> Species -> Species
evalSpecies fitnessFunc s@(i0,max_f0,sum_f0,g0,gs0) = (i0,max_f,sum_f,g0,gs') where
  ((sum_f,max_f),gs') = mapAccumL go (0,0) gs0 --get sum and max fitness of species
  n = fromIntegral $ length gs0 --number of genomes in species
  go :: (Float,Float) -> Genome -> ((Float,Float),Genome)
  go (acc,old_max) g = ((acc + f',new_max),fitness .~ f' $ g) where
    f = fitnessFunc g
    f' = f / n
    new_max = max old_max f'


--produces the next generation of genomes
--assumes the list of genomes is sort in ascending order by fitness
reproduce :: [Float] -> Config -> Int -> Population -> (Int,Population,[Float])
reproduce (r:gen) config gInnov0 population = (gInnov'',population',gen') where
  p_sum_f = foldl' (\a (_,_,f,_,_) -> a + f) 0.0 population --count population sum fitness
  prev_species = map (\(i0,m,a,g,gs) ->
                         let g' = maxFittestGenome (i0,m,a,g,gs)
                         in (i0,m,a,g',[])) -- we need to copy the prev values
                     population --pick random genome from prev generation
  genomes = concat gss
  ((gInnov'',gen'),gss) = mapAccumR (\(gInnov,rs) s@(i0,max_f0,sum_f0,g0,gs0) ->
                                let avg_f = speciesAvgFitness s
                                    num_offspring = round $ sum_f0 / p_sum_f * (fromIntegral $ config^.populationSize)
                                    len = length gs0
                                    stud | len >= 5 = [last gs0]
                                         | otherwise = []
                                    (gInnov',gs',rs') = breedSpecies config gInnov num_offspring gs0 rs
                                in ((gInnov',rs'),gs' ++ stud))
                             (gInnov0,gen) population
  population' = cullEmpty $ speciefy wghtVsTop specThresh prev_species genomes
  wghtVsTop = config^.weightedVsTopology
  specThresh = config^.speciationThreshold

--divides a list of genomes into a list os species
--requires a list of representative species from the last generation
speciefy :: Float -> Float -> [Species] -> [Genome] -> [Species]
speciefy _ _ species [] = species
speciefy wghtVsTop specThresh species (g:gs)
  | isNothing matchIx = speciefy wghtVsTop specThresh ((0,0,0,g,[g]):species) gs
  | otherwise = speciefy wghtVsTop specThresh species' gs
  where
    matchIx = findSpecies wghtVsTop specThresh g species
    i = fromJust matchIx
    (si,sm,ss,sg,sgs) = species !! i
    species' = take i species ++ (si,sm,ss,sg,g:sgs) : drop (i + 1) species

findSpecies :: Float -> Float -> Genome -> [Species] -> Maybe Int
findSpecies wghtVsTop specThresh g = findIndex (\(_,_,_,repG,_) -> geneticDifference wghtVsTop repG g < specThresh)


--removes species whos fitness has stagnated and removes
--the least fit genomes from each species
--assumes genomes are sorted in ascending order by fitness
cull :: Int -> Int -> Population -> Population
cull maxSize maxStag = cullEmpty .
                       map (cullSpecies maxSize) .
                       cullWeakSpecies .
                       filter (\(i,_,_,_,_) -> i < maxStag)

cullWeakSpecies :: Population -> Population
cullWeakSpecies pop = p' where
  p_size = fromIntegral $ length pop
  avg_fs = map speciesAvgFitness pop
  p_avg_f = sum avg_fs
  p' = map snd $ filter (\(avg,s) -> avg / p_avg_f * p_size >= 0.8) (zip avg_fs pop)

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
        excess_disjoint = fromIntegral . length $ union genes1Inn genes2Inn \\ intersect genes1Inn genes2Inn
        genes1Inn = map (^.innovation) . filter (^.enabled) $ g1^.genes
        genes2Inn = map (^.innovation) . filter (^.enabled) $ g2^.genes
        genesBoth = map (getInnovation g1 &&& getInnovation g2) $ intersect genes1Inn genes2Inn
        diff = sum $ map (\(a,b) -> abs $ a^.weight - b^.weight) genesBoth
        num = fromIntegral $ max (g1^.genes.to length) (g2^.genes.to length)

--assumes list of genomes is sorted in ascending order of fitness
breedSpecies :: Config -> Int -> Int -> [Genome] -> [Float] -> (Int,[Genome],[Float])
breedSpecies _ gInnov 0 species rs = (gInnov,[],rs)
breedSpecies config gInnov n species rs = (gInnov'',newChild : otherChildren, rs'')
    where
        (gInnov',newChild,rs') = breedChild config gInnov species rs
        (gInnov'',otherChildren, rs'') = breedSpecies config gInnov' (n-1) species rs'

-- breedChild requires that only genomes IN THE SAME SPECIES are passed
-- assumes list of genomes is sorted in ascending order of fitness
breedChild :: Config -> Int -> [Genome] -> [Float] -> (Int,Genome,[Float])
breedChild _ _ [] _ = error "empty gs"
breedChild config gInnov gs (r:(r1:(r2:rs))) = (gInnov',monster,rs'')
  where
        g1 = getElementR gs r1
        g2 = getElementR gs r2
        (mom,dad) | g1^.fitness >= g2^.fitness = (g1,g2)
                  | otherwise = (g2,g1)
        (child,rs') | r < 0.7 = crossover mom dad rs
                    | otherwise = (getElementR gs r1, r2:rs)
        (gInnov',monster,rs'') = mutate config gInnov child rs'


--assumes the genomes in species are sorted in ascending order
cullSpecies :: Int -> Species-> Species
cullSpecies maxSize (i,m,s,g,gs) = (i,m,s,g,gs')
  where gs' = drop (max (length gs - maxSize) ((length gs `quot` 2) - 1)) gs


--a more general evalute genome?
evaluateGenome :: Config -> [Float] -> Genome -> [Float]
evaluateGenome config inputs (Genome _ maxNode genes) = outs
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
addNode gInnov g0@(Genome _ numnodes genes) (r:rs)
  | null enabledGenes = (gInnov,g0,r:rs)
  | otherwise = (gInnov+2,Genome 0.0 (numnodes+1) genes', rs)
    where
        enabledGenes = filter (^.enabled) genes
        rGene = getElementR enabledGenes r
        newGene1 = set innovation gInnov . set output numnodes $ rGene
        newGene2 = set innovation (gInnov+1) . set weight  1.0 .  set input numnodes $ rGene
        genes' = [newGene1, newGene2, enabled .~ False $ rGene] ++ delete rGene genes

addLink :: Config -> Int -> Genome -> [Float] -> (Int,Genome,[Float])
addLink config gInnov (Genome fit nodes genes) (r:(r1:rs))
  | null disjointPairs = (gInnov,Genome fit nodes genes, r:r1:rs)
  | otherwise = (gInnov+1,Genome fit nodes (newGene : genes), rs)
    where
        inputMax = (config^.numInputs) - 1
        outputMax = inputMax + (config^.numOutputs)
        allPairs = [ (x,y) | x <- [0 .. inputMax] ++ [outputMax + 1 .. nodes]
                           , y <- [inputMax + 1 .. nodes]
                           , x /= y ]
        gPairs = map (\g -> (g^.input,g^.output)) genes
        disjointPairs = allPairs \\ gPairs
        (inp,out) = getElementR disjointPairs r
        newGene = Gene inp out (r1 * 4.0 - 2.0) True gInnov

disableGene :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
disableGene gInnov g0@(Genome fit nodes genes) (r:rs)
  | null enabledGenes = (gInnov,g0,r:rs)
  | otherwise = (gInnov, Genome fit nodes genes', rs)
    where
        enabledGenes = filter (^.enabled) genes
        rGene = getElementR enabledGenes r
        genes' = (enabled .~ False $ rGene) : delete rGene genes

enableGene :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
enableGene gInnov g0@(Genome fit nodes genes) (r:rs)
  | null disabled = (gInnov, g0,r:rs)
  | otherwise = (gInnov, Genome fit nodes genes', rs)
    where
        disabled = filter (not . (^.enabled)) genes
        rGene = getElementR disabled r
        genes' = (enabled .~ True $ rGene) : delete rGene genes

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

-- should mess with these rates of mutation...

-- for large network (HARD real problems, #s should be)
-- 0.1,0.2,0.3,0.5
mutate :: Config -> Int -> Genome -> [Float] -> (Int,Genome,[Float])
mutate config gInnov genome rs = mutateH gInnov (perturbWeights genome rs) (drop (genome^.genes.to length) rs)
  where
    mutateH gInnov genome (r:rs)
      | r < 0.05 = uncurry3 (mutate config) $ addLink config gInnov genome rs
      | r < 0.08 = uncurry3 (mutate config) $ addNode gInnov genome rs
--      | r < 0.012 = uncurry3 (mutate config) $ disableGene gInnov genome rs
--      | r < 0.015 = uncurry3 (mutate config) $ enableGene gInnov genome rs
      | otherwise = (gInnov, genome, r:rs)
    perturbWeights :: Genome -> [Float] -> Genome
    perturbWeights genome rs = genes %~ map (uncurry perturb) . zip rs $ genome
    perturb :: Float -> Gene -> Gene
    perturb r = if smallChange then weight +~ r * 0.4 - 0.2 else weight .~ r * 4.0 - 2.0
      where
        smallChange = floor (r * 100.0) > 4


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
getElementR xs r = xs !! floor ( r * fromIntegral (length xs))

maxFittestSpecies :: Population -> Species
maxFittestSpecies = maximumBy (\(_,a,_,_,_) (_,b,_,_,_) -> compare a b)

maxFittestGenome :: Species -> Genome
maxFittestGenome (_,_,_,_,gs) = maximumBy (\a b -> compare (a^.fitness) (b^.fitness)) gs

fittestGenome :: Population -> Genome
fittestGenome [] = error "empty population"
fittestGenome p = maxFittestGenome . maxFittestSpecies $ p

mapT :: (a -> b) -> (a,a) -> (b,b)
mapT f (a,b) = (f a,f b)

lengthNum :: Num b => [a] -> b
lengthNum = fromIntegral . length

sortSpecies :: Species -> Species
sortSpecies (a,b,c,d,gs) = (a,b,c,d,sortBy (\a b -> compare (a^.fitness) (b^.fitness)) gs)

popSize :: Population -> Int
popSize = sum . map (\(_,_,_,_,gs) -> length gs)

speciesAvgFitness :: Species -> Float
speciesAvgFitness (_,_,s,_,gs) = s / fromIntegral (length gs)


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
fitnessXor config g = let genome_outs = concatMap (flip (evaluateGenome config) g) inputs
                      in (4 - sum (map abs (zipWith (-) outputs genome_outs))) ^ 2

speciesInfo :: Species -> String
speciesInfo (a,b,c,d,e) = "stag[" ++ show a ++ "]\tmax fit[" ++ fmtFloatN b 4 ++
      "]\tsum fit[" ++ fmtFloatN c 4 ++ "]\tsize[" ++ show (length e) ++ "]"
