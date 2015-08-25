{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.Binary
import Numeric


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


data Gene = Gene { _input      :: Int
                 , _output     :: Int
                 , _weight     :: Float
                 , _enabled    :: Bool
                 , _innovation :: Int } deriving (Eq)
makeClassy ''Gene


instance Show Gene where
    show (Gene inp out wt e i)
      | e = "[x]" ++ show i ++ ": " ++ show inp ++ " --> " ++ show wt ++ " --> " ++ show out
      | otherwise = "[ ]" ++ show i


instance Binary Gene where
  put g = do
    put $ g^.input
    put $ g^.output
    put $ g^.weight
    put $ g^.enabled
    put $ g^.innovation
  get = do
    bInput <- get
    bOutput <- get
    bWeight <- get
    bEnabled <- get
    bInnovation <- get
    return $ Gene bInput bOutput bWeight bEnabled bInnovation


data Genome = Genome { _fitness :: Float
                     , _numnodes :: Int
                     , _genes :: [Gene]
                     } deriving (Eq)
makeClassy ''Genome

type Population = [Species]
type Species = ( Int                 --stagnation
               , Float               --max fitness
               , Float               --sum of fitnesses
               , Genome              --representative genome
               , [Genome])           --rest of genomes

instance Show Genome where
  show (Genome f n gs) = "Genome[" ++ fmtFloatN f 4 ++ "] [" ++show n ++
                         "]{" ++ concatMap (\g -> "\n" ++ show g) gs ++ "\n}"


instance Binary Genome where
  put g = do
    put $ g^.fitness
    put $ g^.numnodes
    put . length $ g^.genes
    sequence . map put $ g^.genes --sequence . map put === mapM_ put ?
    return ()

  get = do
    bFitness <- get
    bNodes <- get
    lenGenes <- get :: Get Int
    genes <- sequence [ get | _ <- [1..lenGenes] ]
    return $ Genome bFitness bNodes genes

fmtFloatN :: RealFloat a => a -> Int -> String
fmtFloatN x n = showFFloat (Just n) x ""
