{-# LANGUAGE TemplateHaskell #-}

module NeuralNetwork where

import Emulator
import System.Random
import Data.List
import Data.Map (Map)
import Data.Maybe
import Control.Lens
import qualified Data.Map as Map

data Node = Input Int String | Hidden Int | Output Int String

isInput (Input _ _) = True
isInput _ = False

isOutput (Output _ _) = True
isOutput _ = False

isHidden (Hidden _) = True
isHidden _ = False

getId (Input i _) = i
getId (Hidden i) = i
getId (Output i _) = i

data Gene = Gene { _input      :: Int
                 , _output     :: Int
                 , _weight     :: Float
                 , _enabled    :: Bool
                 , _innovation :: Int } deriving (Eq)
makeClassy ''Gene

data Genome = Genome { _nodes :: [Node]
                     , _genes :: [Gene] }
makeClassy ''Genome

evaluateGenome :: Genome -> [(String,Float)] -> Joystick
evaluateGenome (Genome nodes genes) inputs = fromListJ $ map toButton ["left","right","up","down","b","a"]
    where
        toButton name = (>0.5) . fst . head . filter ((==name) . snd) $ outs
        outs = mapMaybe evaluateOutput nodes
        evaluateOutput x@(Output _ l) = Just (evaluateNode x, l)
        evaluateOutput _ = Nothing
        evaluateNode :: Node -> Float
        evaluateNode (Input _ l) = fromJust . lookup l $ inputs
        evaluateNode n = sum . map evaluateGene . filter isMyGene $ genes
            where
                evaluateGene :: Gene -> Float
                evaluateGene g = g^.weight * sigmoid (evaluateNode (fromJust $ find ((==g^.input) . getId) nodes))
                isMyGene :: Gene -> Bool
                isMyGene g = g^.enabled && g^.output == getId n
-- could use a STATE monad to auto-keep the global innovation up...
addNode :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
addNode gInnov (Genome nodes genes) (r:rs) = (gInnov+2,Genome nodes' genes', rs)
    where
        rGene = getElementR genes r
        newNodeId = maximum . map getId $ nodes
        nodes' = Hidden newNodeId : nodes
        newGene1 = set innovation gInnov . set output newNodeId $ rGene
        newGene2 = set innovation (gInnov+1) . set weight  1.0 .  set input newNodeId $ rGene
        genes' = [newGene1, newGene2, enabled .~ False $ rGene] ++ delete rGene genes

addLink :: Int -> Genome -> [Float] -> (Int,Genome,[Float])
addLink gInnov (Genome nodes genes) (r:(r1:rs)) = (gInnov+1,Genome nodes (newGene : genes), rs)
    where
        allPairs = [ (getId x,getId y) | x <- nodes, y <- nodes ]
        gPairs = map (\g -> (g^.input,g^.output)) genes
        disjointPairs = allPairs \\ gPairs
        (inp,out) = getElementR disjointPairs r
        newGene = Gene inp out (r1 * 4.0 - 2.0) True gInnov


{-
-- 90% chance to make a small change
-- 10% chance to reset completely
smallScale = 0.2
largeScale = 2.0
crossOverChance = 0.75

-- this presupposes that the fitness of network1 is greater than that of network2
-- since if we don't crossover we just mutate the first
crossover :: Genome -> Genome -> [Float] -> (Genome, [Float])
crossover n@(Genome nodes genes) n'@(Genome nodes' genes') (c:(r:rs)) = mutate newNet (drop (length nodes) rs)
    where
       newNet 
         | c < 0.75 = foldl' (switchGenome n') n $ zip (Map.keys nodes) rs
         | otherwise = Network nodes edges

deleteLinks _ [] = []
deleteLinks nId ((x,y):xs)
  | x == nId || y == nId = deleteLinks nId xs
  | otherwise      =  (x,y) : deleteLinks nId xs

switchGenome (Network fromNodes fromEdges) (Network toNodes toEdges) (nId,r) = if r < 0.5 then Network toNodes toEdges else Network newNodes newEdges
    where
        newNodes  = Map.insert nId (fromNodes Map.! nId) toNodes
        nodeEdges = filter (\(x,y) -> x == nId || y == nId) fromEdges
        -- we might just want to add the new edges...
        -- since otherwise we are modifying other genes??
        -- in the original each only had 1 input / output
        -- i guess meaning each gene was a node
        -- tied together with two edges, with the node
        -- as the fst of one and the snd of the other
        -- which we might want to replicate......
        -- i mean it really depends on the difference between edges??
        --
        -- cause in the current construction we could be deleting things we shouldn't
        -- and if we just add links we might just be adding links each generation without a way to delete
        -- maybe edges should also keep track of which node "owns" the edge?
        newEdges  = nodeEdges ++ deleteLinks nId toEdges -- is this the proper construction? cause we're going to delete some edges and alter other genes too... so order will matter...

mutate :: Network -> [Float] -> (Network,[Float])
mutate n@(Network nodes edges) (r:rs)
  | r < 0.1 = let (edges',rs') = mutateLinkA n rs in mutate (Network nodes edges') rs'
  | r < 0.2 = let (edges',rs') = mutateLinkD n rs in mutate (Network nodes edges') rs'
  | r < 0.4 = let (nodes',rs') = mutateWeight nodes rs in mutate (Network nodes' edges) rs'
  | otherwise = (Network nodes edges,rs)

mutateWeight :: Map Int Node -> [Float] -> (Map Int Node,[Float])
mutateWeight nodes (c:(p':(i:rs))) = (Map.insert (uid n) n nodes, rs)
    where
        p = p' * 4.0 - 2.0 -- [-2.0,2.0)
        n = getElementR (Map.elems nodes) i
        w' = if c < 0.9
              then
                weight n + smallScale * p
              else
                largeScale * p


linkExists _ _ [] = False
linkExists x y ((x1,y1):xs) = uid x == x1 && uid y == y1 || uid y == x1 && uid x == y1 || linkExists x y xs

mutateLinkA :: Network -> [Float] -> ([(NodeId,NodeId)],[Float])
mutateLinkA (Network nodes edges) (i1:(i2:(order:rs))) = (edges ++ edges',rs)
    where
        n1 = getElementR (Map.elems nodes) i1
        n2 = getElementR (Map.elems nodes) i2
        edges' 
         | uid n1 == uid n2 || (nType n1 == Input "" && nType n2 == Input "") || linkExists n1 n2 edges = []
         | nType n1 == Input "" || order < 0.5 = [(uid n1,uid n2)]
         | otherwise = [(uid n2,uid n1)] -- we're gonna create a connection n1 -> n2

mutateLinkD :: Network -> [Float] -> ([(NodeId,NodeId)],[Float])
mutateLinkD (Network nodes edges) (i:rs) = (if keep then edges else delete (n1,n2) edges,rs)
    where
        (n1,n2) = getElementR edges i
        node1 = nodes Map.! n1
        node2 = nodes Map.! n2
        -- don't want to remove the only thing coming from an input...
        keep = ((nType node1 == Input "") && count1 == 1) || ((nType node2 == Output "") && count2 == 1)
        count1 = length . filter ((==n1) . fst) $ edges
        count2 = length . filter ((==n2) . snd) $ edges
-}
sigmoid :: Float -> Float
sigmoid x = 2.0 / (1.0 + exp (-4.9 * x)) - 1.0

getElementR :: [a] -> Float -> a
getElementR xs r = xs !! floor ( r * fromIntegral (length xs))
