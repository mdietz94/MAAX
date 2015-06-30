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
    let (gInnov,p) = createPopulation (mkStdGen 0) (config^.populationSize) (config^.numInputs) (config^.numOutputs)
    --print $ fittestGenome p
    let f = fitnessXor config
    let p' = run (mkStdGen 0) config f gInnov p 100
    let g0 = fittestGenome p'
    print $ length p'
    print g0
    print $ f g0

-- make global for easier repl-ing
(gInnov,p) = createPopulation (mkStdGen 0) (xorConfig^.populationSize) (xorConfig^.numInputs) (xorConfig^.numOutputs)
f = fitnessXor xorConfig
p' = run (mkStdGen 0) xorConfig f gInnov p 100
g0 = fittestGenome p'
