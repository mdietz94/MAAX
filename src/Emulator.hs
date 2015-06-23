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

    data Joystick = Joystick { right  :: Bool
                             , left   :: Bool
                             , down   :: Bool
                             , up     :: Bool
                             , start  :: Bool
                             , select :: Bool
                             , b      :: Bool
                             , a      :: Bool }


    joystickToChar :: Joystick -> CUChar
    joystickToChar (Joystick r l d u st s b a) = fromListBE [r,l,d,u,st,s,b,a]


    {-

    Basically we just have to call Create, passing the name of the ROM and a pointer to our joystick data.
    The pointer must be kept alive throughout the lifetime of the emulator, and must be updated with the
    current joystick data !before! running Step or StepFull.


    -}

    foreign import ccall "Create" create :: (CString -> (Ptr CUChar) -> IO ())
    foreign import ccall "Destroy" destroy :: (IO ())
    foreign import ccall "Save" save :: (Ptr CUChar)
    foreign import ccall "Load" load :: (IO ())
    foreign import ccall "GetMemory" getMemory :: (Ptr CUChar)
    foreign import ccall "GetImage" getImage :: (Ptr CUChar)
    foreign import ccall "Step" step :: (IO ())
    foreign import ccall "StepFull" stepFull :: (IO ())
    foreign import ccall "RamChecksum" ramChecksum :: (IO CLong)
    foreign import ccall "ImageChecksum" imageChecksum :: (IO CLong)
