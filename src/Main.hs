import Emulator
import qualified Data.ByteString as B (writeFile)

main = do
    saveData <- runProgram "superMario.nes" (const defaultJoystick) 10
    B.writeFile "super.sav" saveData
