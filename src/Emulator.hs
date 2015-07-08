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

    data Color = Color { red :: Word8, green :: Word8, blue :: Word8, alpha :: Word8 }

    {-

    Basically we just have to call Create, passing the name of the ROM and a pointer to our joystick data.
    The pointer must be kept alive throughout the lifetime of the emulator, and must be updated with the
    current joystick data !before! running Step or StepFull.


    -}

    runProgram :: String -> ([Word8] -> Joystick) -> Int -> IO ByteString
    runProgram name func frames = do
        ptr <- malloc
        create name ptr
        mem <- getMemory
        saveData <- loopProgram ptr func mem frames
        destroy
        return saveData
        where
            loopProgram _ _ _ 0 = save
            loopProgram ptr f mem n = do
                mem' <- step ptr (f mem)
                loopProgram ptr f mem' (n-1)

    create :: String -> Ptr CUChar -> IO ()
    create name ptr = withCString name $ flip createC ptr

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

    step :: Ptr CUChar -> Joystick -> IO [Word8]
    step ptr j = poke ptr (joystickToChar j) >> stepC >> getMemory

    stepFull :: Ptr CUChar -> Joystick -> IO [Word8]
    stepFull ptr j = poke ptr (joystickToChar j) >> stepFullC >> getMemory

    load :: ByteString -> IO ()
    load saveRAW = useAsCString saveRAW (loadC . castPtr)

    foreign import ccall "Create" createC :: (CString -> Ptr CUChar -> IO ())
    foreign import ccall "Destroy" destroy :: (IO ())
    foreign import ccall "Save" saveC :: (Ptr (Ptr CUChar) -> IO CLong)
    foreign import ccall "Load" loadC :: (Ptr CUChar -> IO ())
    foreign import ccall "GetMemory" getMemoryC :: (IO (Ptr CUChar))
    foreign import ccall "GetImage" getImageC :: (IO (Ptr CUChar))
    foreign import ccall "Step" stepC :: (IO ())
    foreign import ccall "StepFull" stepFullC :: (IO ())
    foreign import ccall "RamChecksum" ramChecksum :: (IO CLong)
    foreign import ccall "ImageChecksum" imageChecksum :: (IO CLong)
