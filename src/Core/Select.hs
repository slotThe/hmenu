module Core.Select
    ( runUpdate
    , formatUserPaths
    , getExecutables
    , makeNewEntries
    , selectWith
    , sortByValues
    , tryRead
    ) where

-- Local imports
import Core.Parser (getHist)
import Core.Toml (Config(Config, files, open, term, tty))
import Core.Util
    ( Items
    , OpenIn(Open, Term)
    , getSearchPath
    , histFile
    , openWith
    , spawn
    , tryAddPrefix
    )

-- ByteString
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Map
import qualified Data.Map.Strict as Map

-- Other imports
import Data.Bool (bool)
import Data.List (sortBy)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Posix.Directory.Traversals (getDirectoryContents)
import System.Posix.Files.ByteString (fileExist)
import System.Process (proc)
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
    histFile >>= (`BS.writeFile` showItems update)
  where
    -- | Update the value of a particular key by just adding one to it.
    updateValueIn :: ByteString -> Items -> Items
    updateValueIn = Map.adjust succ

    -- | Pretty print our items.
    showItems :: Items -> ByteString
    showItems = BS.unlines . map showItem . Map.toList

    -- | Pretty print a single (application, score) tuple.
    showItem :: (ByteString, Int) -> ByteString
    showItem (k, v) = k <> " " <> BS.pack (show v)

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

    pure $ case exitCode of
        -- Take first (= selected) word or return the error message.
        ExitSuccess   -> Right $ BS.takeWhile (/= '\n') sOut
        ExitFailure i -> Left (i, sErr)

-- | Try to read a file that contains a map.  Return an empty map if the file
-- doesn't exist.
tryRead :: FilePath -> IO Items
tryRead file =
    bool (pure Map.empty)
         (tryParseFile file)
         =<< doesFileExist file
  where
    tryParseFile :: FilePath -> IO Items
    tryParseFile f = do
        hist <- getHist f
        pure $ case hist of
            Left  _   -> Map.empty
            Right kvl -> Map.fromList kvl  -- key-value-list

-- | Get all executables from all dirs in $PATH.
getExecutables :: IO [ByteString]
getExecutables = fmap concat . traverse listExistentDir =<< getSearchPath

{- | Only try listing the directory if it actually exists.
   This is for all the people who have non-existent dirs in their path for some
   reason.
-}
listExistentDir :: ByteString -> IO [ByteString]
listExistentDir fp =
    bool (pure [])
         (getDirContents fp)
          =<< fileExist fp
  where
    getDirContents =
        fmap (filter (`notElem` [".", ".."]) . map snd) . getDirectoryContents

-- | Decide what to actually do with the user selection from dmenu.
decideSelection :: ByteString -> Config -> ByteString
decideSelection sel Config{ files, tty, term, open }
    | sel `elem` files = openWith Open open sel
    | sel `elem` tty   = openWith Term term sel
    | otherwise        = sel

-- | Turn a list into 'Items' and set all starting values to 0.
makeNewEntries :: [ByteString] -> Items
makeNewEntries xs = Map.fromList [(x, 0) | x <- xs]

-- | Sort 'Items' by its values and return the list of keys.
-- This will make often used commands bubble up to the top.
sortByValues :: Items -> [ByteString]
sortByValues it =
    map fst
        . sortBy (\(_, a) (_, b) -> compare b a)
        $ Map.toList it

-- | Process user defined files, add the appropriate prefixes if needed.
formatUserPaths
    :: ByteString    -- ^ Prefix for '$HOME'.
    -> [ByteString]  -- ^ User defined strings for option.
    -> [ByteString]  -- ^ Properly formatted paths.
formatUserPaths home = map (tryAddPrefix home)
