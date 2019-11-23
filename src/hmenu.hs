module Main where

-- Local imports
import Core.Toml
import Core.Util (clean, splitOnColon, tryAddPrefix)

-- DMenu
import           DMenu (MonadDMenu, (.=))
import qualified DMenu

-- Other imports
import Control.Monad (void)
import Control.Monad.Trans (MonadIO)
import Data.Bool (bool)
import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf, sort)
import System.Directory (doesPathExist, listDirectory)
import System.Environment (getEnv)
import System.Process (spawnCommand)


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

-- | execute dmenu and then do stuff.
main :: IO ()
main = do
    home <- userHome
    exe  <- getExecutables
    Config{ files, filePrefix, open } <- getUserConfig

    -- Get all executables and interweave it with the users configuration.
    let getExes = userConfig home filePrefix files <> exe

    -- Remove duplicates, then sort.
    let exes = sort . nubOrd $ getExes

    -- Let the user select something from the list.
    selection <- DMenu.select setOptions exes

    -- Process output.
    case selection of
        Left  _ -> return () -- silently fail
        -- TODO I should probably handle this with dedicated types.
        Right s -> if
            | filePrefix `isPrefixOf` s -> spawn . open . clean $ s
            | otherwise                 -> spawn s

-- | Options for dmenu.
-- FIXME: This is kind of a hack to allow me to use user-defined stuff.
setOptions :: (MonadIO m, MonadDMenu m) => m ()
setOptions = do
    -- Get user set configuration.
    Config { numLines
           , caseIns
           , font
           , normBgCol
           , normFgCol
           , selBgCol
           , selFgCol
           } <- getUserConfig

    DMenu.numLines        .= numLines
    DMenu.caseInsensitive .= caseIns
    DMenu.font            .= font
    DMenu.normalBGColor   .= normBgCol
    DMenu.normalFGColor   .= normFgCol
    DMenu.selectedBGColor .= selBgCol
    DMenu.selectedFGColor .= selFgCol
