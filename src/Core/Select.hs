module Core.Select
    ( -- * Interacting with dmenu
      selectWith       -- :: [String] -> [ByteString] -> String -> IO (Either ProcessError ByteString)

      -- * Interacting with the history file
    , tryRead          -- :: FilePath -> IO Items
    , runUpdate        -- :: ByteString -> Config -> Items -> IO ()

      -- * Interacting with the system
    , getExecutables   -- :: IO [ByteString]
    , evalDirs         -- :: [ByteString] -> IO [ByteString]

      -- * Pretty printing
    , showItems        -- :: Items -> ByteString

      -- * Utility
    , makeNewEntries   -- :: [ByteString] -> Items
    , sortByValues     -- :: Items -> [ByteString]
    ) where

import Core.Parser (getHist)
import Core.Toml (Config (Config, decay, files, histPath, open, term, tty))
import Core.Util hiding ((</>))

import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict       qualified as Map

import Data.Double.Conversion.ByteString (toShortest)
import System.Posix.Directory.ByteString (closeDirStream, openDirStream)
import System.Posix.Directory.Foreign (DirType, dtDir, dtUnknown)
import System.Posix.Directory.Traversals (readDirEnt, traverseDirectoryContents)
import System.Posix.Env.ByteString (getEnvDefault)
import System.Posix.FilePath (RawFilePath, (</>))
import System.Posix.Files.ByteString (fileExist, getFileStatus, isDirectory)
import System.Process.ByteString (readCreateProcessWithExitCode)


{- | When a spawned process fails, this type is used to represent the
exit code and @stderr@ output.

See <https://github.com/m0rphism/haskell-dmenu/blob/master/src/DMenu/Run.hs>.
-}
type ProcessError :: Type
type ProcessError = (Int, ByteString)

-- | Do the appropriate things with the user selection and update the
-- history file.
runUpdate
    :: ByteString  -- ^ What the user picked.
    -> Config      -- ^ User config containing things that interest us.
    -> Items       -- ^ Map prior to selection.
    -> IO ()
runUpdate selection cfg@Config{ histPath, decay } itemMap = do
    spawn $ decideSelection selection cfg     -- execute selected command
    histPath `BS.writeFile` showItems update  -- update map and write to file
  where
    -- Update the items based on the users selection.
    update :: Items = Map.adjust (+ 1) selection (Map.map (* decay) itemMap)

{- | Run dmenu with the given command line options and a list of entries
from which the user should choose.

   Originally <https://github.com/m0rphism/haskell-dmenu/blob/master/src/DMenu/Run.hs select>.
-}
selectWith
    :: [String]     -- ^ List of options to give to dmenu.
    -> [ByteString] -- ^ List of executables from which the user should select.
    -> String       -- ^ The dmenu executable.
    -> IO (Either ProcessError ByteString)
    -- ^ The selection made by the user, or a 'ProcessError', if the user
    -- canceled.
selectWith opts entries dmenu = do
    -- Spawn the process with the available options and entries.
    (exitCode, sOut, sErr) <-
        readCreateProcessWithExitCode (proc dmenu opts) (BS.unlines entries)

    pure case exitCode of
        -- Take first (= selected) word or return the error message.
        ExitFailure i -> Left (i, sErr)
        ExitSuccess   -> Right $! BS.takeWhile (/= '\n') sOut

-- | Try to read a file that contains a map.  Return an empty map if the
-- file doesn't exist.
tryRead :: FilePath -> IO Items
tryRead file = ifM (doesFileExist file) (getHist file) (pure mempty)

-- | Get all executables from all dirs in $PATH.
getExecutables :: IO [ByteString]
getExecutables = fmap concat
               . traverse listExecutables
               . BS.split ':'
             =<< getEnvDefault "PATH" ""

-- | List all executables in the given directory. Only try listing the
-- directory if it actually exists; this is necessary, since people may
-- have non-existent things in their path.
listExecutables :: ByteString -> IO [ByteString]
listExecutables dir = ifM (fileExist dir) (getDirContents dir) (pure [])
  where
    getDirContents :: ByteString -> IO [ByteString]
    getDirContents =
        traverseDirectoryContents
            (\xs (dt, name) -> isDirDT dt name <&> \d -> if d then xs else name : xs)
            []

-- | Apply 'evalDir' to some list of file paths.
evalDirs :: [ByteString] -> IO [ByteString]
evalDirs = fmap concat . traverse evalDir

-- | If the given file path is a directory, try to list all of its
-- contents, but *don't* do so recursively.  Otherwise just return the
-- file path as is.
evalDir :: ByteString -> IO [ByteString]
evalDir dir = do
    -- Try to make the path absolute, as 'listDir' can only handle
    -- absolute paths.
    home <- getEnvDefault "HOME" ""
    let absPath = if   "~/" `BS.isPrefixOf` dir
                  then home </> BS.drop 2 dir
                  else dir
    ifM (isDir absPath)
        -- List all things inside the directory and restore the original
        -- naming scheme.
        (map (dir </>) <$> listExecutables absPath)
        (pure [dir])

-- | Check if the given file path is a directory.
isDir :: RawFilePath -> IO Bool
isDir fp = catch
    do (dt, _) <- bracket (openDirStream fp) closeDirStream readDirEnt
       isDirDT dt fp
    \(_ :: SomeException) -> pure False

-- | Check if the given file path is a directory. The given directory
-- type @dt@ is assumed to be the actual one for @fp@.
isDirDT :: DirType -> RawFilePath -> IO Bool
isDirDT dt fp
    | dt == dtDir     = pure True
    | dt == dtUnknown = isDirectory <$> getFileStatus fp
    | otherwise       = pure False

-- | Pretty print our items.
showItems :: Items -> ByteString
showItems = BS.unlines . map showItem . toList
  where
    -- Pretty print a single (application, score) tuple.
    showItem :: (ByteString, Double) -> ByteString
    showItem (k, v) = k <> " " <> toShortest v

-- | Decide what to actually do with the user selection from dmenu.
decideSelection :: ByteString -> Config -> ByteString
decideSelection sel Config{ files, tty, term, open }
    | sel `elem` files = openWith (Open open) sel
    | sel `elem` tty   = openWith (Term term) sel
    | otherwise        = sel

-- | Turn a list into 'Items' and set all starting values to 0.
makeNewEntries :: [ByteString] -> Items
makeNewEntries = Map.fromSet (const 0) . fromList

-- | Sort 'Items' by its values and return the list of keys.
-- This will make often used commands bubble up to the top.
sortByValues :: Items -> [ByteString]
sortByValues = map fst . sortBy (flip compare `on` snd) . toList
