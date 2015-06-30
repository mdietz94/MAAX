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
    let (gInnov,p) = createPopulation (mkStdGen 0) 10 2 1
    print $ fittestGenome p
    let p' = run (mkStdGen 0) config (fitnessXor (config ^. maxLinkLength) (config ^. numInputs) (config ^. numOutputs)) gInnov p 10
    --print $ fittestGenome p'
    print p'
