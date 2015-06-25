module NeuralNetwork where

import Emulator
import System.Random
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

data NodeType = Input String | Default | Output String
instance Eq NodeType where
   (Input _) == (Input _) = True
   Default == Default = True
   (Output _) == (Output _) = True
   _ == _ = False


data Node = Node { f :: Float -> Float
                 , weight :: Float
                 , nType :: NodeType
                 , uid :: NodeId }

type NodeId = Int

instance Show Node where
    show (Node _ _ _ x) = "<Node: " ++ show x ++ ">"

data Network = Network { nodes :: Map NodeId Node, edges :: [(NodeId,NodeId)] }

fromNodeList :: [Node] -> Map NodeId Node
fromNodeList = Map.fromList . map (\n -> (uid n, n))

-- we will take as input some slices of RAM
-- and we will output the keys to press.

-- we should probably also take a few other things
-- like some info about our past moves, for instance
-- in order to avoid getting stuck at local maxima

-- or maybe that's worth just using time travel to get
-- around.  For instance moving right if we're stuck against a
-- wall may require moving left, which is a rare move, and might
-- only be noticeable if we think we are 'stuck' but we can probably

runNetwork :: Network -> [(Float,String)] -> Joystick
runNetwork (Network nodes edges) inputs = fromListJ $ map toButton ["left","right","up","down","b","a"]
    where
        toButton name = (>0.5) . fst . head . filter ((==name) . snd) $ outs
        outs = map (evaluateNode . (nodes Map.!) . snd) . nub . filter ((== Output "") . nType . (nodes Map.!) . snd) $ edges
        evaluateNode :: Node -> (Float,String)
        evaluateNode (Node f w (Input label) _)  = (w * f i, "")
            where
                i = fst . head . filter ((==label) . snd) $ inputs
        evaluateNode (Node f w Default n)        = (sum (map (fst . evaluateNode) (lastLayer n)), "")
        evaluateNode (Node f w (Output label) n) = (sum (map (fst . evaluateNode) (lastLayer n)), label)
        lastLayer n = map ((nodes Map.!) . fst) $ filter ((==n) . snd) edges

{-

Basically what we want to do is create some randos
and then score based on 'x' position + 'next world' (hahah that would be nice)
+ score - time + lives and that will be score.  We will take the best ones,
and merge them.

Simply put, we can create a variety of different formats, try them all, merge weights of best same format.
All of them will have the same number of inputs and outputs, so it will be the number / links on the 2 hidden layers. (at start we should avoid from back-linking?)
Then when we merge them, we take the better one, and keep all its shit, then we take the 2nd one, and where it has ones with the same id, 50% chance to replace

We also mutate at each generation, we can do a few things
    a) change weight
    b) take 2 random nodes and create a link as long as (both aren't input) and this doesn't create a loop
    c) turn a node into 2 new nodes
    d) merge 2 nodes ... low chance ...
-}

-- 90% chance to make a small change
-- 10% chance to reset completely
smallScale = 0.2
largeScale = 2.0
crossOverChance = 0.75

-- this presupposes that the fitness of network1 is greater than that of network2
-- since if we don't crossover we just mutate the first
breedChild :: Network -> Network -> [Float] -> (Network, [Float])
breedChild n@(Network nodes edges) n'@(Network nodes' edges') (c:(r:rs)) = mutate newNet (drop (length nodes) rs)
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

getElementR :: [a] -> Float -> a
getElementR xs r = xs !! floor ( r * fromIntegral (length xs))

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

sigmoid :: Float -> Float
sigmoid x = 2.0 / (1.0 + exp (-4.9 * x)) - 1.0
