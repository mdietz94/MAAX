import Emulator
import NeuralNetwork
import System.Random
import qualified Data.ByteString as B (writeFile)

-- main = do
--     saveData <- runProgram "superMario.nes" (const defaultJoystick) 10
--     B.writeFile "super.sav" saveData

main :: IO ()
main = do
    let p = createPopulation (mkStdGen 0) 10 1 2 1
    print $ fittestGenome p
    let p' = run (mkStdGen 0) fitnessXor p 10
    print $ fittestGenome p
    return ()
