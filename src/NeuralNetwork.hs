module NeuralNetwork where

import Emulator
import System.Random
import Data.List

data NodeType = Input Float | Default | Output String
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

instance Eq Node where
    (Node _ _ _ x) == (Node _ _ _ y) = x == y

instance Show Node where
    show (Node _ _ _ x) = "<Node: " ++ show x ++ ">"

data Network = Network { nodes :: [Node], edges :: [(NodeId,NodeId)] }

-- we will take as input some slices of RAM
-- and we will output the keys to press.

-- we should probably also take a few other things
-- like some info about our past moves, for instance
-- in order to avoid getting stuck at local maxima

-- or maybe that's worth just using time travel to get
-- around.  For instance moving right if we're stuck against a
-- wall may require moving left, which is a rare move, and might
-- only be noticeable if we think we are 'stuck' but we can probably

getNode :: [Node] -> NodeId -> Node
getNode ns nId = head $ filter (\n -> uid n == nId) ns

runNetwork :: Network -> [Float] -> Joystick
runNetwork (Network nodes edges) inputs = fromListJ $ map toButton ["left","right","up","down","b","a"]
    where
        toButton name = (>0.5) . fst . head . filter (\(_,l) -> l == name) $ outs
        outs = map (evaluateNode . getNode nodes . snd) $ filter ((== Output "") . nType . getNode nodes . snd) edges
        evaluateNode (Node f w (Input i) _)      = (w * f i, "")
        evaluateNode (Node f w Default n)        = (sum (map (fst . evaluateNode) (lastLayer n)), "")
        evaluateNode (Node f w (Output label) n) = (sum (map (fst . evaluateNode) (lastLayer n)), label)
        lastLayer :: NodeId -> [Node]
        lastLayer n = map (\(x,_) -> head $ filter (\(Node _ _ _ i) -> x == i) nodes) $ filter (\(_,e) -> e == n) edges

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
         | c < 0.75 = foldl' (switchGenome n') n $ zip (map uid nodes) rs
         | otherwise = Network nodes edges

switchGenome (Network fromNodes fromEdges) (Network toNodes toEdges) (nId,r) = if r < 0.5 then Network toNodes toEdges else Network newNodes newEdges
    where
        newNode   = getNode fromNodes nId
        nodeEdges = filter (\(x,y) -> x == nId || y == nId) fromEdges
        newNodes  = newNode : delete newNode toNodes
        newEdges  = nodeEdges ++ filter (\(x,y) -> x /= nId && y /= nId) toEdges

mutate :: Network -> [Float] -> (Network,[Float])
mutate (Network nodes edges) (r:rs)
  | r < 0.1 = let (edges',rs') = mutateLinkA nodes edges rs in mutate (Network nodes edges') rs'
  | r < 0.2 = let (edges',rs') = mutateLinkD nodes edges rs in mutate (Network nodes edges') rs'
  | r < 0.4 = let (nodes',rs') = mutateWeight nodes rs in mutate (Network nodes' edges) rs'
  | otherwise = (Network nodes edges,rs)

mutateWeight nodes (c:(p':(i':rs))) = (take i nodes ++ [n] ++ drop (i+1) nodes, rs)
    where
        p = p' * 4.0 - 2.0
        i = floor $ i' * fromIntegral  (length nodes)
        n = nodes !! i
        w' = if c < 0.9
              then
                weight n + smallScale * p
              else
                largeScale * p

mutateLinkA :: [Node] -> [(NodeId,NodeId)] -> [Float] -> ([(NodeId,NodeId)],[Float])
mutateLinkA nodes edges (i1:(i2:(order':rs))) = (edges ++ edges',rs)
    where
        n1 = nodes !! floor ( i1 * fromIntegral (length nodes))
        n2 = nodes !! floor ( i2 * fromIntegral (length nodes))
        order = order' < 0.5
        edges' 
         | n1 == n2 || (nType n1 == Input 0 && nType n2 == Input 0) || ((uid n1,uid n2) `elem` edges) || ((uid n2,uid n1) `elem` edges) = []
         | nType n1 == Input 0 || order = [(uid n1,uid n2)]
         | otherwise = [(uid n2,uid n1)] -- we're gonna create a connection n1 -> n2

mutateLinkD :: [Node] -> [(NodeId,NodeId)] -> [Float] -> ([(NodeId,NodeId)],[Float])
mutateLinkD nodes edges (i:rs) = (edges',rs)
    where
        (n1,n2) = edges !! floor (i * fromIntegral (length edges))
        Just node1 = find (\(Node _ _ _ x) -> x == n1) nodes
        Just node2 = find (\(Node _ _ _ x) -> x == n2) nodes
        -- don't want to remove the only thing coming from an input...
        remove = not $ ((nType node1 == Input 0) && count1 == 1) || ((nType node2 == Output "") && count2 == 1)
        count1 = length . filter ((==n1) . fst) $ edges
        count2 = length . filter ((==n2) . snd) $ edges
        edges' = if remove then delete (n1,n2) edges else edges
