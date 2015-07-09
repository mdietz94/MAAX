import Emulator
import NeuralNetwork
import System.Random
import Control.Lens
import qualified Data.ByteString as B (writeFile, pack)
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import Foreign.Ptr
import Foreign.Marshal.Alloc
import qualified Vision.Image.Storage.DevIL as F
import qualified Vision.Image as F
import Vision.Primitive

getToTheGame n
 | n == 0 = stepFull defaultJoystick
 | n < 5 = stepFull (Joystick False False False False True False False False) >> getToTheGame (n-1)
 | n < 10 = stepFull defaultJoystick >> getToTheGame (n-1)
 | n < 20 = stepFull (Joystick False False False False True False False False) >> getToTheGame (n-1)
 | otherwise = stepFull defaultJoystick >> getToTheGame (n-1)

getToTheGameRecord n
 | n == 0 = [defaultJoystick]
 | n < 5 = (Joystick False False False False True False False False) : getToTheGameRecord (n-1)
 | n < 10 = defaultJoystick : getToTheGameRecord (n-1)
 | n < 20 = (Joystick False False False False True False False False) : getToTheGameRecord (n-1)
 | otherwise = defaultJoystick : getToTheGameRecord (n-1)

runMario genome = do
  create "superMario.nes"
  getToTheGame 100
  mem <- getMemory
  memFinal <- loop (10*60) mem
--  img <- getImage
--  let imgRAW = F.fromFunction (Z :. 256 :. 256) (ptToPix img) :: F.RGBA
--  F.save (F.PNG) "screenGrab.png" imgRAW
  destroy
  putStrLn . ("My Fitness: "++) . show . calculateFitness . map fromIntegral $ memFinal
  return ( fitness .~ calculateFitness (map fromIntegral memFinal) $ genome )
    where
      loop 0 _ = getMemory
      loop n mem = stepFull (outputs mem) >>= loop (n-1)
      outputs mem = fromListJ . map (>0.5) $ evaluateGenome marioConfig (getInputs . map fromIntegral $  mem) genome

runMarioSpecies :: Species -> IO Species
runMarioSpecies (s,m,fit,rep,gen) = do
  gen' <- sequence . map runMario $ gen
  let n = fromIntegral $ length gen'
  let sum_f = foldl (\a g -> a + g^.fitness) 0.0 gen'
  let max_g = maximumBy (\a b -> compare (a^.fitness) (b^.fitness)) gen'
  return (s,max_g^.fitness,sum_f,max_g,gen')

stepMarioNetwork (gInnov,p0,gen) = do
  p1 <- sequence . map runMarioSpecies $ p0
  let stagnant = zipWith (<=) (map (\(_,m,_,_,_) -> m) p1) (map (\(_,m,_,_,_) -> m) p0)
  let p2 = zipWith (\a (s,m,fit,rep,gen) -> if a then (s+1,m,fit,rep,gen) else (0,m,fit,rep,gen)) stagnant p1
  let p3 = map sortSpecies p2
  putStrLn . show $ p3
  let p4 = cull (marioConfig^.speciesMaxSize) (marioConfig^.stagnationMax) p3
  putStrLn . ("Top Fitness: " ++) . show . (^.fitness) . fittestGenome $ p4
  return ( reproduce gen marioConfig gInnov p4 )

runMarioNetwork 0 (_,p0,_) = return p0
runMarioNetwork n st = runMarioNetwork (n-1) =<< stepMarioNetwork st

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
      loop n mem = step (outputs mem) >>= loop (n-1) >>= (\xs -> return $ (outputs mem) : xs)
      outputs mem = fromListJ . map (>0.5) $ evaluateGenome marioConfig (getInputs . map fromIntegral $  mem) genome

ptToPix img (Z :. y :. x) = F.RGBAPixel r g b a
  where
    Color r g b a = img !! (256*y+x)

main = do
  let gen = randomRs (0.0,1.0) $ mkStdGen 23
  let (gInnov,p0) = initPopulation gen marioConfig
  finalPop <- runMarioNetwork 32 (gInnov,p0,gen)
  let bestGenome = fittestGenome $ finalPop
  putStrLn . ("Top Fitness: "++) . show . (^.fitness) $ bestGenome
  putStrLn . show $ bestGenome
  joydata <- recordMario bestGenome
  saveAsFM2 "best.fm2" joydata

{-
   Here is some specific Mario code.  Meant to be used in tested Neural Network before we get the ability to get data from the screen.
-}

marioX :: [Int] -> Int
marioX mem = (mem!!0x6D) * 0x100 + (mem!!0x86)

marioY :: [Int] -> Int
marioY mem = (mem!!0x03B8) + 16

screenX :: [Int] -> Int
screenX = (!!0x03AD)

screenY :: [Int] -> Int
screenY = (!!0x03B8)

getTile :: [Int] -> (Int,Int) -> Bool
getTile mem (dx,dy) = res
  where
    x = (marioX mem) + dx - 8
    y = (marioY mem) + dy - 16
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
  let dist = map (\(x,y) -> (abs $ x - (marioX mem)+dx, abs $ y - (marioY mem)+ dy)) (getSprites mem)
  let sprite = not . null . filter (\(distx,disty) -> distx < 8 && disty < 8) $ dist
  return $ if tile && (marioY mem) + dy < 0x1B0 then 1.0 else (if sprite then -1.0 else 0.0)

calculateFitness :: [Int] -> Float
calculateFitness mem = fromIntegral $ if marX > 3186 then marX + 1000 else marX
  where marX = marioX mem
