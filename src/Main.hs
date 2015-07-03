import Emulator
import NeuralNetwork
import System.Random
import Control.Lens
import qualified Data.ByteString as B (writeFile)

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
    let p' = run rs config f gInnov p 10
    let g0 = fittestGenome p'
    print g0
    print $ f g0

-- make global for easier repl-ing
--(gInnov,p) = initPopulation (mkStdGen 0) xorConfig
--f = fitnessXor xorConfig
--p' = run (mkStdGen 0) xorConfig f gInnov p 100
--g0 = fittestGenome p'
