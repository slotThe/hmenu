module Main where

-- Local imports
import Core.Toml (dmenuExe, Config(Config), filePrefix, files, getUserConfig, open)
import Core.Util (clean, splitOnColon, tryAddPrefix)

-- Other imports
import Control.Monad (void)
import Data.Bool (bool)
import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf, sort)
import Data.String (unlines)
import System.Directory (doesPathExist, listDirectory)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (proc, readCreateProcessWithExitCode, spawnCommand)


{- | When a spawned process fails, this type is used to represent the exit code
   and @stderr@ output.

   See: https://github.com/m0rphism/haskell-dmenu/blob/master/src/DMenu/Run.hs
-}
type ProcessError = (Int, String)

-- | PATH
path :: IO FilePath
path = getEnv "PATH"

-- | HOME
userHome :: IO FilePath
userHome = (++ "/") <$> getEnv "HOME"

-- | User defined files.
userConfig
    :: FilePath  -- ^ Prefix for $HOME.
    -> FilePath  -- ^ Prefix for an option.
    -> [String]  -- ^ User defined strings for option.
    -> [String]  -- ^ Properly formatted names.
userConfig home pref = map (addPrefix . tryAddPrefix home)
  where
    addPrefix = (pref ++)

-- | Get all executables from all dirs in `$PATH`.
getExecutables :: IO [String]
getExecutables = fmap concat . traverse listExistentDir . splitOnColon =<< path

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
    -> IO (Either ProcessError String)
    -- ^ The selection made by the user, or a 'ProcessError', if the user
    -- canceled.
selectWith opts entries = do
    -- Get the user specified executable.
    -- Default: "dmenu"
    Config{ dmenuExe } <- getUserConfig

    -- TODO properly comment this
    (exitCode, sOut, sErr) <-
        readCreateProcessWithExitCode
            (proc dmenuExe opts)
            (unlines entries)

    return $ case exitCode of
        -- Take first (selected) word or return the error message.
        ExitSuccess   -> Right $ takeWhile (/= '\\') sOut
        ExitFailure i -> Left (i, sErr)

-- | Execute dmenu and then do stuff.
main :: IO ()
main = do
    opts <- getArgs
    home <- userHome
    exe  <- getExecutables
    Config{ files, filePrefix, open } <- getUserConfig

    -- Get all executables and interweave it with the users configuration.
    let getExes = userConfig home filePrefix files <> exe

    -- Remove duplicates, then sort.
    let exes = sort . nubOrd $ getExes

    -- Let the user select something from the list.
    selection <- selectWith opts exes

    -- Process output.
    case selection of
        Left  _ -> return () -- silently fail
        -- TODO I should probably handle this with dedicated types.
        Right s -> if
            | filePrefix `isPrefixOf` s -> spawn . open . clean $ s
            | otherwise                 -> spawn s
