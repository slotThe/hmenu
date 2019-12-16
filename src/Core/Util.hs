module Core.Util
    ( ShowBS
    , tryAddPrefix
    , spawn
    , openIn
    , hmenuPath
    , histFile
    ) where

-- ByteString
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Qualified other imports
import qualified System.Posix.FilePath as BS -- used for ByteString version of </>

-- Other imports
import Control.Monad (void)
import Data.Functor ((<&>))
import System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)
import System.FilePath ((</>))
import System.Process (spawnCommand)


-- | ShowS for ByteString because it is shorter :>.
type ShowBS = ByteString -> ByteString

{- | Add a prefix to a string if the string is not starting with "/".
   This will ensure the user can specify absolute paths to files, but also
   conveniently use relative paths (starting from '$HOME') if that is desired.
-}
tryAddPrefix :: ByteString -> ByteString -> ByteString
tryAddPrefix prefix xs
    | BS.null xs  = ""
    | isSpecial x = xs
    | otherwise   = prefix BS.</> xs
  where
    x         = BS.head xs
    isSpecial = (`elem` ['/', '~'])

-- | Spawn a command and forget about it.
spawn :: ByteString -> IO ()
spawn = void . spawnCommand . BS.unpack

-- | Open something in a specified terminal.
openIn :: ShowBS -> ByteString -> ByteString
openIn term cmd = term $ " -e " <> cmd

-- | XDG_CONFIG_HOME
xdgConfig :: IO FilePath
xdgConfig = getXdgDirectory XdgConfig ""

-- | Path to the hmenu directory.
-- 'XDG_CONFIG_HOME\/hmenu', so probably '~\/.config\/hmenu'.
hmenuPath :: IO FilePath
hmenuPath = xdgConfig <&> (</> "hmenu")

-- | Path to the history file.
-- '~\/.config\/hmenu\/histFile'
histFile :: IO FilePath
histFile = hmenuPath <&> (</> "histFile")
