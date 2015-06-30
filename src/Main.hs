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
    let f = fitnessXor (config ^. maxLinkLength) (config ^. numInputs) (config ^. numOutputs)
    let p' = run (mkStdGen 0) config f gInnov p 30
    let g = fittestGenome p'
    print $ length p'
    print $ f g
