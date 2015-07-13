{-# LANGUAGE ForeignFunctionInterface #-}
module Emulator where
    import Data.List
    import Data.Word
    import Foreign.Ptr
    import Foreign.Storable
    import Foreign.C.Types
    import Foreign.C.String
    import Foreign.Marshal.Array
    import Foreign.Marshal.Alloc
    import Data.Bits.Bitwise (fromListBE)
    import Data.ByteString (pack, ByteString, useAsCString)

    data Joystick = Joystick { right  :: Bool
                             , left   :: Bool
                             , down   :: Bool
                             , up     :: Bool
                             , start  :: Bool
                             , select :: Bool
                             , b      :: Bool
                             , a      :: Bool }

    defaultJoystick = Joystick False False False False False False False False

    joystickToChar :: Joystick -> CUChar
    joystickToChar (Joystick r l d u st s b a) = fromListBE [r,l,d,u,st,s,b,a]

    fromListJ :: [Bool] -> Joystick
    fromListJ [r,l,d,u,b,a] = Joystick r l d u False False b a
    fromListJ [r,l,d,u,st,se,b,a] = Joystick r l d u st se b a

    toListJ :: Joystick -> [Bool]
    toListJ (Joystick r l d u t s b a) = [r,l,d,u,t,s,b,a]

    data Color = Color { red :: Word8, green :: Word8, blue :: Word8, alpha :: Word8 }

    {-

    Basically we just have to call Create, passing the name of the ROM and a pointer to our joystick data.
    The pointer must be kept alive throughout the lifetime of the emulator, and must be updated with the
    current joystick data !before! running Step or StepFull.


    -}

    marioHeader = "version 3\nemuVersion 22020\nrerecordCount 0\npalFlag 0\nromFilename superMario\nromChecksum base64:jjYwGG411HcjG/j9UOVM3Q==\nguid 986DE381-4E6D-16EF-451A-A243D2F5BA26\nfourscore 0\nmicrophone 0\nport0 1\nport1 1\nport2 0\nFDS 0\nNewPPU 0\nsavestate base64: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\n"


    toFM2String :: [Joystick] -> String
    toFM2String js = marioHeader ++ (concatMap (\j -> "|0|" ++ (jToString j) ++ "|" ++ (jToString j) ++ "||\n") js)
      where
        jToString j = map (\(c,b) -> if b then c else '.') $ zip "RLDUTSBA" (toListJ j)

    saveAsFM2 :: String -> [Joystick] -> IO ()
    saveAsFM2 name = writeFile name . toFM2String

    runProgram :: String -> ([Word8] -> Joystick) -> Int -> IO ByteString
    runProgram name func frames = do
        create name
        mem <- getMemory
        saveData <- loopProgram func mem frames
        destroy
        return saveData
        where
            loopProgram _ _ 0 = save
            loopProgram f mem n = do
                mem' <- step (f mem)
                loopProgram f mem' (n-1)

    create :: String -> IO ()
    create name = withCString name createC

    save :: IO ByteString
    save = do
        ptr <- malloc
        len <- saveC ptr
        dataPtr <- peek ptr -- now this is the pointer to the data we want
        arr <- peekArray (fromIntegral len) (castPtr dataPtr)
        return $ pack arr

    getMemory :: IO [Word8]
    getMemory = do
        ptr <- getMemoryC
        let dPtr = castPtr ptr
        peekArray 0x800 dPtr

    getImage :: IO [Color]
    getImage = do
        ptr <- getImageC
        arr <- peekArray (256*256*4) (castPtr ptr)
        return $ toColorList arr

    toColorList :: [Word8] -> [Color]
    toColorList [] = []
    toColorList [r,g,b,a] = [Color r g b a]
    toColorList xs = toColorList (take 4 xs) ++ toColorList (drop 4 xs)

    step :: Joystick -> IO [Word8]
    step j = stepC (joystickToChar j) >> getMemory

    stepFull :: Joystick -> IO [Word8]
    stepFull j = stepFullC (joystickToChar j) >> getMemory

    load :: ByteString -> IO ()
    load saveRAW = useAsCString saveRAW (loadC . castPtr)

    foreign import ccall "Create" createC :: (CString -> IO ())
    foreign import ccall "Destroy" destroy :: (IO ())
    foreign import ccall "Save" saveC :: (Ptr (Ptr CUChar) -> IO CLong)
    foreign import ccall "Load" loadC :: (Ptr CUChar -> IO ())
    foreign import ccall "GetMemory" getMemoryC :: (IO (Ptr CUChar))
    foreign import ccall "GetImage" getImageC :: (IO (Ptr CUChar))
    foreign import ccall "Step" stepC :: (CUChar -> IO ())
    foreign import ccall "StepFull" stepFullC :: (CUChar -> IO ())
    foreign import ccall "RamChecksum" ramChecksum :: (IO CLong)
    foreign import ccall "ImageChecksum" imageChecksum :: (IO CLong)
