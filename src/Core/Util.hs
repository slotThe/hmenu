module Core.Util
    ( tryAddPrefix
    ) where

-- ByteString
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Other imports
import System.Posix.FilePath ((</>))


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
