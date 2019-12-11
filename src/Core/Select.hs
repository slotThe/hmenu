{-# LANGUAGE TypeApplications #-}

module Core.Select
    ( decideSelection
    , formatUserPaths
    , getExecutables
    , makeNewEntries
    , selectWith
    , sortByValues
    , tryRead
    ) where

-- Local imports
import Core.Toml (Config(Config), filePrefix, histFile, open)
import Core.Util (clean, tryAddPrefix)

-- Map
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Other imports
import Control.Monad (void)
import Data.Bool (bool)
import Data.List (isPrefixOf, sortBy)
import System.Directory (doesFileExist, doesPathExist, listDirectory)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (getSearchPath)
import System.Process (proc, readCreateProcessWithExitCode, spawnCommand)


{- | When a spawned process fails, this type is used to represent the exit code
   and @stderr@ output.
   See: https://github.com/m0rphism/haskell-dmenu/blob/master/src/DMenu/Run.hs
-}
type ProcessError = (Int, String)

-- | Type for a single item, a list of which will be fed to dmenu.
type Item = Map String Int

-- | Decide what to actually do with the user selection from dmenu.
decideSelection
    :: String  -- ^ What the user picked.
    -> Config  -- ^ User defined configuration.
    -> Item    -- ^ Map prior to selection.
    -> IO ()
decideSelection selection Config{ filePrefix, open } itemMap = do
    -- Adjust the value based on the users selection.
    let update = Map.adjust succ selection itemMap

    -- TODO I should probably handle this with dedicated types.
    if | filePrefix `isPrefixOf` selection -> spawn . open . clean $ selection
       | otherwise -> spawn selection

    -- Write the new map to the hist file.
    histFile >>= (`writeFile` show update)

{- | Run dmenu with the given command line optinos and a list of entries from
   which the user should choose.

   Originally 'select' in here:
       https://github.com/m0rphism/haskell-dmenu/blob/master/src/DMenu/Run.hs
-}
selectWith
    :: [String]
    -- ^ List of options to give to dmenu.
    -> [String]
    -- ^ List from which the user should select.
    -> String
    -- ^ The dmenu executable.
    -> IO (Either ProcessError String)
    -- ^ The selection made by the user, or a 'ProcessError', if the user
    -- canceled.
selectWith opts entries dmenu = do
    -- Spawn the process with the available options and entries.
    (exitCode, sOut, sErr) <-
        readCreateProcessWithExitCode (proc dmenu opts) (unlines entries)

    pure $ case exitCode of
        -- Take first (selected) word or return the error message.
        ExitSuccess   -> Right $ takeWhile (/= '\n') sOut
        ExitFailure i -> Left (i, sErr)


-- | Try to read a file that contains a map.  Return an empty map if the file
-- doesn't exist.
tryRead :: FilePath -> IO Item
tryRead file =
    bool (pure Map.empty)
         (read @Item <$> readFile file)
         =<< doesFileExist file

-- | Get all executables from all dirs in '$PATH'.
getExecutables :: IO [FilePath]
getExecutables =
    fmap concat . traverse listExistentDir =<< getSearchPath

{- | Only try listing the directory if it actually exists.
   This is for all the people who have non-existent dirs in their path for some
   reason.
-}
listExistentDir :: FilePath -> IO [FilePath]
listExistentDir fp =
    bool (pure [])
         (listDirectory fp)
          =<< doesPathExist fp

-- | spawn a command and forget about it.
spawn :: String -> IO ()
spawn = void . spawnCommand

-- | Turn a list into an 'Item' and set all starting values to 0.
makeNewEntries :: [String] -> Item
makeNewEntries xs = Map.fromList [(x, 0) | x <- xs]

-- | Sort an item by its values and return the list of keys.
sortByValues :: Item -> [String]
sortByValues it =
    map fst
        . sortBy (\(_,a) (_,b) -> compare b a)
        $ Map.toList it

-- | Process user defined files, add the appropriate prefixes if needed.
formatUserPaths
    :: FilePath    -- ^ Prefix for '$HOME'.
    -> String      -- ^ Prefix for an option.
    -> [FilePath]  -- ^ User defined strings for option.
    -> [FilePath]  -- ^ Properly formatted paths.
formatUserPaths home pref = map (addPrefix . tryAddPrefix home)
  where
    addPrefix = (pref ++)
