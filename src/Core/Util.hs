module Core.Util
    ( ShowBS
    , OpenIn(Term, Open)
    , Items
    , tryAddPrefix
    , spawn
    , openWith
    , hmenuPath
    , histFile
    , getSearchPath
    ) where

-- Map
import Data.Map.Strict (Map)

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
import System.Posix.Env.ByteString (getEnvDefault)
import System.Process (spawnCommand)


-- | Type for an Map that describes all of the executables with their ratings.
type Items = Map ByteString Int

-- | Type for helping to decide how to open something.
data OpenIn
    = Term
    | Open

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

-- | Open something.
openWith
    :: OpenIn      -- ^ Decide what to add to the opening function.
    -> ShowBS      -- ^ How to open something.
    -> ByteString  -- ^ The thing to open.
    -> ByteString
openWith Term t = t . (" -e " <>)
openWith Open o = o . (" "    <>)

-- | Get all directories in @\$PATH@ as a list.
getSearchPath :: IO [ByteString]
getSearchPath = BS.split ':' <$> getEnvDefault "PATH" ""

-- | XDG_CONFIG_HOME
xdgConfig :: IO FilePath
xdgConfig = getXdgDirectory XdgConfig ""

-- | Path to the hmenu directory.
-- @XDG_CONFIG_HOME\/hmenu@, so probably @~\/.config\/hmenu@.
hmenuPath :: IO FilePath
hmenuPath = xdgConfig <&> (</> "hmenu")

-- | Path to the history file.
-- @~\/.config\/hmenu\/histFile@
histFile :: IO FilePath
histFile = hmenuPath <&> (</> "histFile")
