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
import Core.Toml (Config(Config, files, histPath, open, term, tty))
import Core.Util (Items, OpenIn(Open, Term), openWith, spawn)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as Map
import qualified Data.Set              as Set

import System.Posix.Directory.ByteString (openDirStream)
import System.Posix.Directory.Foreign (DirType, dtDir, dtUnknown)
import System.Posix.Directory.Traversals (getDirectoryContents, readDirEnt)
import System.Posix.Env.ByteString (getEnvDefault)
import System.Posix.FilePath ((</>))
import System.Posix.Files.ByteString (fileExist, getFileStatus, isDirectory)
import System.Process.ByteString (readCreateProcessWithExitCode)


{- | When a spawned process fails, this type is used to represent the exit code
   and @stderr@ output.
   See: https://github.com/m0rphism/haskell-dmenu/blob/master/src/DMenu/Run.hs
-}
type ProcessError = (Int, ByteString)

-- | Do the appropriate things with the user selection and update the history
-- file.
runUpdate
    :: ByteString  -- ^ What the user picked.
    -> Config      -- ^ User config containing things that interest us.
    -> Items       -- ^ Map prior to selection.
    -> IO ()
runUpdate selection cfg itemMap = do
    -- Adjust the value based on the users selection.
    let update = selection `updateValueIn` itemMap

    -- Based on what the user selected, execute the appropriate command.
    spawn $ decideSelection selection cfg

    -- Write the new map to the hist file.
    histPath cfg `BS.writeFile` showItems update
  where
    -- | Update the value of a particular key by just adding one to it.
    updateValueIn :: ByteString -> Items -> Items
    updateValueIn = Map.adjust succ

{- | Run dmenu with the given command line optinos and a list of entries from
   which the user should choose.

   Originally 'select' in here:
       https://github.com/m0rphism/haskell-dmenu/blob/master/src/DMenu/Run.hs
-}
selectWith
    :: [String]
    -- ^ List of options to give to dmenu.
    -> [ByteString]
    -- ^ List of executables from which the user should select.
    -> String
    -- ^ The dmenu executable.
    -> IO (Either ProcessError ByteString)
    -- ^ The selection made by the user, or a 'ProcessError', if the user
    -- canceled.
selectWith opts entries dmenu = do
    -- Spawn the process with the available options and entries.
    (exitCode, sOut, sErr) <-
        readCreateProcessWithExitCode (proc dmenu opts) (BS.unlines entries)

    pure $! case exitCode of
        -- Take first (= selected) word or return the error message.
        ExitFailure i -> Left (i, sErr)
        ExitSuccess   -> Right $! BS.takeWhile (/= '\n') sOut

-- | Try to read a file that contains a map.  Return an empty map if the file
-- doesn't exist.
tryRead :: FilePath -> IO Items
tryRead file = ifM (doesFileExist file) (getHist file) (pure mempty)

-- | Get all executables from all dirs in $PATH.
getExecutables :: IO [ByteString]
getExecutables = fmap concat
               . traverse listDir
               . BS.split ':'
             =<< getEnvDefault "PATH" ""

{- | Only try listing the directory if it actually exists.
   This is for all the people who have non-existent dirs in their path for some
   reason.
-}
listDir :: ByteString -> IO [ByteString]
listDir dir = ifM (fileExist dir) (getDirContents dir) (pure [])
  where
    getDirContents :: ByteString -> IO [ByteString]
    getDirContents = fmap (map snd . removeDirs) . getDirectoryContents

    removeDirs :: [(DirType, ByteString)] -> [(DirType, ByteString)]
    removeDirs = filter \(dt, name) -> dt /= dtDir && name `notElem` [".", ".."]

-- | Apply 'evalDir' to some list of file paths.
evalDirs :: [ByteString] -> IO [ByteString]
evalDirs dirs = concat <$> traverse evalDir dirs

-- | If the given file path is a directory, try to list all of its contents.
-- Otherwise just return the file path as is.
evalDir :: ByteString -> IO [ByteString]
evalDir dir = do
    -- Try to make the path absolute, as 'getDirectoryContents' can only
    -- handle absolute paths.
    home <- getEnvDefault "HOME" ""
    let absPath = if   "~/" `BS.isPrefixOf` dir
                  then home </> BS.drop 2 dir
                  else dir
    ifM (isDir absPath)
        -- List all things inside the directory and restore the original naming scheme.
        (map (dir </>) <$> listDir absPath)
        (pure [dir])

-- | Check if something is a directory.
isDir :: ByteString -> IO Bool
isDir fp = catch
    do dirStream  <- openDirStream fp
       (dt, name) <- readDirEnt dirStream
       if | dt == dtDir     -> pure True
          | dt == dtUnknown -> isDirectory <$> getFileStatus (fp </> name)
          | otherwise       -> pure False
    \(_ :: SomeException) -> pure False

-- | Pretty print our items.
showItems :: Items -> ByteString
showItems = BS.unlines . map showItem . Map.toList
  where
    -- | Pretty print a single (application, score) tuple.
    showItem :: (ByteString, Int) -> ByteString
    showItem (k, v) = k <> " " <> BS.pack (show v)

-- | Decide what to actually do with the user selection from dmenu.
decideSelection :: ByteString -> Config -> ByteString
decideSelection sel Config{ files, tty, term, open }
    | sel `elem` files = openWith Open open sel
    | sel `elem` tty   = openWith Term term sel
    | otherwise        = sel

{- | Turn a list into 'Items' and set all starting values to 0.
   NOTE: The implementation using sets seems to perform slightly better memory
         wise than the naive implementation @ fromList [(x, 0) | x <- xs] @.
-}
makeNewEntries :: [ByteString] -> Items
makeNewEntries xs = Map.fromSet (const 0) $ Set.fromList xs

-- | Sort 'Items' by its values and return the list of keys.
-- This will make often used commands bubble up to the top.
sortByValues :: Items -> [ByteString]
sortByValues it = map fst
                . sortBy (\(_, a) (_, b) -> compare b a)
                $ Map.toList it
