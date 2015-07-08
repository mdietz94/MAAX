import Emulator
import NeuralNetwork
import System.Random
import Control.Lens
import qualified Data.ByteString as B (writeFile)
import Data.Maybe (fromJust)

-- main = do
--     saveData <- runProgram "superMario.nes" (const defaultJoystick) 10
--     B.writeFile "super.sav" saveData

main :: IO ()
main = do
    let config = xorConfig
    let rs = randomRs (0,0.99999999) (mkStdGen 0)
    let (gInnov,p) = initPopulation rs config
    print $ fittestGenome p
    let f = fitnessXor config
    let p' = run rs config f gInnov p 30
    let g0 = fittestGenome p'
    print g0
    print $ f g0

-- make global for easier repl-ing
--(gInnov,p) = initPopulation (mkStdGen 0) xorConfig
--f = fitnessXor xorConfig
--p' = run (mkStdGen 0) xorConfig f gInnov p 100
--g0 = fittestGenome p'

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

getInputs :: [Int] -> [Int]
getInputs mem = do --[ if (getTile mem (dx,dy)) == 1 && (marioY mem)+dy < 0x1B0 then 1 else | dx <- [-6*16,-5*16..6*16], dy <- [-6*16,-5*16..6*16] ]
  dx <- [-6*16,-5*16..6*16]
  dy <- [-6*16,-5*16..6*16]
  let tile = getTile mem (dx,dy)
  let dist = map (\(x,y) -> (abs $ x - (marioX mem)+dx, abs $ y - (marioY mem)+ dy)) (getSprites mem)
  let sprite = not . null . filter (\(distx,disty) -> distx < 8 && disty < 8) $ dist
  return $ if tile && (marioY mem) + dy < 0x1B0 then 1 else (if sprite then -1 else 0)
