module NeuralNetwork where

import Emulator
import System.Random
import Prelude hiding (id)
import Data.List (delete)

data NodeType = Input Float | Default | Output String
instance Eq NodeType where
   (Input _) == (Input _) = True
   Default == Default = True
   (Output _) == (Output _) = True
   _ == _ = False


data Node = Node { f :: Float -> Float
                 , weight :: Float
                 , nType :: NodeType
                 , id :: Int }

instance Eq Node where
    (Node _ _ _ x) == (Node _ _ _ y) = x == y

instance Show Node where
    show (Node _ _ _ x) = "<Node: " ++ show x ++ ">"

data Network = Network { nodes :: [Node], edges :: [(Node,Node)] }

-- we will take as input some slices of RAM
-- and we will output the keys to press.

-- we should probably also take a few other things
-- like some info about our past moves, for instance
-- in order to avoid getting stuck at local maxima

-- or maybe that's worth just using time travel to get
-- around.  For instance moving right if we're stuck against a
-- wall may require moving left, which is a rare move, and might
-- only be noticeable if we think we are 'stuck' but we can probably

runNetwork :: Network -> [Float] -> Joystick
runNetwork (Network nodes edges) inputs = fromListJ $ map toButton ["left","right","up","down","b","a"]
    where
        toButton name = (>0.5) . fst . head . filter (\(_,l) -> l == name) $ outs
        outs = map (evaluateNode . snd) $ filter (\(_,e) -> nType e == Output "") edges
        evaluateNode   (Node f w (Input i) _)      = (w * f i, "")
        evaluateNode n@(Node f w Default _)        = (sum (map (fst . evaluateNode) (lastLayer n)), "")
        evaluateNode n@(Node f w (Output label) _) = (sum (map (fst . evaluateNode) (lastLayer n)), label)
        lastLayer n = map fst $ filter (\(_,e) -> e == n) edges

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

mutateWeight (Node f w n i) g = (Node f w' n i, g'')
    where
        (c,g') = random g :: (Float,StdGen)
        (p,g'') = randomR (-2.0,2.0) g
        w' = if c < 0.9
              then
                w + smallScale * p
              else
                largeScale * p

mutateLinkA :: [Node] -> [(Node,Node)] -> StdGen -> ([(Node,Node)],StdGen)
mutateLinkA nodes edges g = (edges ++ edges',g''')
    where
        (i1,g')  = randomR (0,length nodes - 1) g
        n1 = nodes !! i1
        (i2,g'') = randomR (0,length nodes - 1) g
        n2 = nodes !! i2
        (order,g''') = randomR (False, True) g''
        edges' 
         | (id n1 == id n2) || (nType n1 == Input 0 && nType n2 == Input 0) || ((n1,n2) `elem` edges) || ((n2,n1) `elem` edges) = []
         | nType n1 == Input 0 || order = [(n1,n2)]
         | otherwise = [(n2,n1)] -- we're gonna create a connection n1 -> n2

mutateLinkD :: [Node] -> [(Node,Node)] -> StdGen -> ([(Node,Node)],StdGen)
mutateLinkD nodes edges g = (edges',g')
    where
        (i,g') = randomR (0,length edges - 1) g
        (n1,n2) = edges !! i
        -- don't want to remove the only thing coming from an input...
        remove = not $ ((nType n1 == Input 0) && count1 == 1) || ((nType n2 == Output "") && count2 == 1)
        count1 = length . filter (\(x,_) -> x == n1) $ edges
        count2 = length . filter (\(_,y) -> y == n2) $ edges
        edges' = if remove then delete (n1,n2) edges else edges
