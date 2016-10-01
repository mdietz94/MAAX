module Mario where

import Emulator
import Types
import NeuralNetwork
import System.Random
import Control.Lens
import qualified Data.ByteString as B (writeFile, pack)
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Control.Exception (AsyncException(..),catch,throw)

getToTheGame n
 | n == 0 = stepFull defaultJoystick
 | n < 5 = stepFull (Joystick False False False False True False False False) >> getToTheGame (n-1)
 | n < 10 = stepFull defaultJoystick >> getToTheGame (n-1)
 | n < 20 = stepFull (Joystick False False False False True False False False) >> getToTheGame (n-1)
 | otherwise = stepFull defaultJoystick >> getToTheGame (n-1)

getToTheGameRecord n
 | n == 0 = [defaultJoystick]
 | n < 5 = Joystick False False False False True False False False : getToTheGameRecord (n-1)
 | n < 10 = defaultJoystick : getToTheGameRecord (n-1)
 | n < 20 = Joystick False False False False True False False False : getToTheGameRecord (n-1)
 | otherwise = defaultJoystick : getToTheGameRecord (n-1)
 
drawImage :: String -> IO ()
drawImage name = do
  img <- getImage
  writePPM (name ++ ".ppm") 256 256 img

writePPM :: String -> Int -> Int -> [Color] -> IO ()                            
writePPM name w h pixels = writeFile name txt where
  toTxt :: Word8 -> String
  toTxt = (++ " ") . show . (fromIntegral :: Word8 -> Int)
  f :: [Color] -> String
  f [] = ""
  f (Color r g b _:ps) = toTxt r ++ toTxt g ++ toTxt b ++ f ps
  txt = "P3\n" ++ show w ++ " " ++ show h ++ " 255\n" ++ f pixels


runMario :: Genome -> IO Genome
runMario genome = do
  create "superMario.nes"
  getToTheGame 100
  mem <- getMemory
  ftns <- loop (10*60) mem 0.0
  destroy
  putStrLn . ("My Fitness: "++) . show $ ftns
  return ( fitness .~ ftns $ genome ) -- calculateFitness (map fromIntegral memFinal) $ genome )
    where
      loop 0 _ f = return f
      loop n mem m_fit = do
        mem' <- stepFull (outputs mem)
        if marioAlive (map fromIntegral mem')
         then do
              let fit = calculateFitness . map fromIntegral $ mem'
              loop (n-1) mem' (max fit m_fit)
         else loop 0 mem' m_fit
      outputs mem = fromListJ . map (>0.5) $ evaluateGenome marioConfig (getInputs . map fromIntegral $  mem) genome

runMarioSpecies :: Species -> IO Species
runMarioSpecies (s,m,fit,rep,gen) = do
  gen' <- mapM runMario gen
  let sum_f = foldl (\a g -> a + g^.fitness) 0.0 gen'
  let max_g = maximumBy (\a b -> compare (a^.fitness) (b^.fitness)) gen'
  return (s,max_g^.fitness,sum_f,max_g,gen')

stepMarioNetwork (gInnov,p0,gen) = do
  p1 <- mapM runMarioSpecies p0
  let stagnant = zipWith (<=) (map (\(_,m,_,_,_) -> m) p1) (map (\(_,m,_,_,_) -> m) p0)
  let p2 = zipWith (\a (s,m,fit,rep,gen) -> if a then (s+1,m,fit,rep,gen) else (0,m,fit,rep,gen)) stagnant p1
  let p3 = map sortSpecies p2
  print p3
  let p4 = cull (marioConfig^.speciesMaxSize) (marioConfig^.stagnationMax) p3
  putStrLn . ("Top Fitness: " ++) . show . (^.fitness) . fittestGenome $ p4
  return ( reproduce gen marioConfig gInnov p4 )


stepNetwork p0 (gInnov,p1,gen) config = reproduce gen config gInnov p5
  where
    p2 = map calculateSpecies p1
    stagnant = zipWith (<=) (map (\(_,m,_,_,_) -> m) p1) (map (\(_,m,_,_,_) -> m) p0)
    p3 = zipWith (\a (s,m,fit,rep,gen) -> if a then (s+1,m,fit,rep,gen) else (0,m,fit,rep,gen)) stagnant p2
    p4 = map sortSpecies p3
    p5 = cull (config^.speciesMaxSize) (config^.stagnationMax) p4
    calculateSpecies (s,m,fit,rep,gen) = (s,max_g^.fitness,sum_f,max_g,gen)
      where
        sum_f = foldl (\a g -> a + g^.fitness) 0.0 gen
        max_g = maximumBy (\a b -> compare (a^.fitness) (b^.fitness)) gen


runMarioNetwork 0 (_,p0,_) = return p0
runMarioNetwork n st@(_,p0,_) = (runMarioNetwork (n-1) =<< stepMarioNetwork st) `catch` handleUserInterrupt
  where
    handleUserInterrupt UserInterrupt = return p0
    handleUserInterrupt e = throw e

recordMario genome = do
  create "superMario.nes"
  let startData = getToTheGameRecord 100
  getToTheGame 100
  mem <- getMemory
  outs <- loop (30*60) mem
  destroy
  return $ startData ++ outs
    where
      loop 0 _ = return []
      loop n mem = step (outputs mem) >>= loop (n-1) >>= (\xs -> return $ outputs mem : xs)
      outputs mem = fromListJ . map (>0.5) $ evaluateGenome marioConfig (getInputs . map fromIntegral $  mem) genome


marioMain = do
  let gen = randomRs (0.0,1.0) $ mkStdGen 23
  let (gInnov,p0) = initPopulation gen marioConfig
  finalPop <- runMarioNetwork 32 (gInnov,p0,gen)
  let bestGenome = fittestGenome finalPop
  putStrLn . ("Top Fitness: "++) . show . (^.fitness) $ bestGenome
  savePopulation "last_population.bin" finalPop
  joydata <- recordMario bestGenome
  saveAsFM2 "best.fm2" joydata

loadPop = do
  p0 <- loadPopulation "last_population.bin"
  let gInnov = maximum . map _innovation  .concatMap _genes . concatMap (\(_,_,_,_,gs) -> gs) $ p0
  let gen = randomRs (0.0,1.0) $ mkStdGen 23
  finalPop <- runMarioNetwork 32 (gInnov,p0,gen)
  let bestGenome = fittestGenome finalPop
  putStrLn . ("Top Fitness: "++) . show . (^.fitness) $ bestGenome
  savePopulation "last_population.bin" finalPop
  joydata <- recordMario bestGenome
  saveAsFM2 "best.fm2" joydata

{-
   Here is some specific Mario code.  Meant to be used in tested Neural Network before we get the ability to get data from the screen.
-}

marioX :: [Int] -> Int
marioX mem = (mem!!0x6D) * 0x100 + toSigned (mem!!0x86)
  where
    toSigned x = if x < 128 then x else x - 256

marioY :: [Int] -> Int
marioY mem = (mem!!0x03B8) + 16

marioAlive :: [Int] -> Bool
marioAlive mem = (mem!!0x075A) == 2

screenX :: [Int] -> Int
screenX = (!!0x03AD)

screenY :: [Int] -> Int
screenY = (!!0x03B8)

getTile :: [Int] -> (Int,Int) -> Bool
getTile mem (dx,dy) = res
  where
    x = marioX mem + dx - 8
    y = marioY mem + dy - 16
    page = (x `quot` 256) `rem` 2
    subx = (x `rem` 256) `quot` 16
    suby = (y - 32) `quot` 16
    addr = 0x500 + page * 13 * 16 + suby * 16 + subx
    res
      | suby >= 13 || suby < 0 = False
      | mem!!addr /= 0 = True
      | otherwise = False

getSprites :: [Int] -> [(Int,Int)]
getSprites mem = map fromJust . filter (/=Nothing) $ enemies
  where
    enemies = [ if mem!!(0xF + slot) /= 0 then Just (ex slot,ey slot) else Nothing | slot <- [0..4] ]
    ex slot = mem!!(0x6E + slot) * 0x100 + mem!!(0x87+slot)
    ey slot = mem!!(0xCF + slot) + 24

getInputs :: [Int] -> [Float]
getInputs mem = do
  dx <- [-6*16,-5*16..6*16]
  dy <- [-6*16,-5*16..6*16]
  let tile = getTile mem (dx,dy)
  let dist = map (\(x,y) -> (abs $ x - marioX mem + dx, abs $ y - marioY mem + dy)) (getSprites mem)
  let sprite = any (\(distx,disty) -> distx < 8 && disty < 8) dist
  return $ if tile && marioY mem + dy < 0x1B0 then 1.0 else (if sprite then -1.0 else 0.0)

calculateFitness :: [Int] -> Float
calculateFitness mem = fromIntegral $ if marX > 3186 then marX + 1000 else marX
  where marX = marioX mem
