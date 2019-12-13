module Core.Util
    ( tryAddPrefix
    , spawn
    ) where

-- ByteString
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Other imports
import Control.Monad (void)
import System.Posix.FilePath ((</>))
import System.Process (spawnCommand)

{- | Add a prefix to a string if the string is not starting with "/".
   This will ensure the user can specify absolute paths to files, but also
   conveniently use relative paths (starting from '$HOME') if that is desired.
-}
tryAddPrefix :: ByteString -> ByteString -> ByteString
tryAddPrefix prefix xs
    | BS.null xs = ""
    | xIsSpecial = xs
    | otherwise  = prefix </> xs
  where
    xIsSpecial = x `elem` ['/', '~']
    x = BS.head xs

-- | Spawn a command and forget about it.
spawn :: ByteString -> IO ()
spawn = void . spawnCommand . BS.unpack
