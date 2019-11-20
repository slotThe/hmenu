module Main where

-- Local imports
import Core.Toml (Config(Config), filePrefix, files, getUserConfig, open)
import Core.Util (addPrefix, clean, splitOnColon)

-- DMenu
import           DMenu (Color(..), MonadDMenu, (.=))
import qualified DMenu

-- Other imports
import Control.Monad (void)
import Data.Bool (bool)
import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf, sort)
import System.Directory (doesPathExist, listDirectory)
import System.Environment (getEnv)
import System.Process (spawnCommand)


-- | PATH
path :: IO String
path = getEnv "PATH"

-- | HOME
userHome :: IO String
userHome = (++ "/") <$> getEnv "HOME"

-- | Add user defined configuration to *something* (this will be the stuff
-- extracted from path).
-- TODO This looks rather ugly
addConfigWith
    :: String   -- ^ Home prefix
    -> String   -- ^ Config prefix
    -> [String] -- ^ User defined config
    -> [String] -- ^ executables from path
    -> [String] -- ^ everything thrown together
addConfigWith = (((++) .) .) . userFiles

-- | User defined files.
userFiles
    :: String   -- ^ "home" prefix
    -> String   -- ^ cfg prefix
    -> [String] -- ^ User defined configs
    -> [String]
userFiles home pref = map ((pref ++) . addPrefix home)

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
    let getExes = addConfigWith home filePrefix files exe

    -- Remove duplicates, then sort.
    let exes = sort . nubOrd $ getExes

    -- Let the user select something from the list.
    selection <- DMenu.select setOptions exes

    -- Process output.
    case selection of
        Left  _ -> return () -- silently fail
        -- TODO I should probably handle this with dedicated types.
        Right s -> if
            | filePrefix `isPrefixOf` s -> spawn . (open ++) . clean $ s
            | otherwise                 -> spawn s

-- | Options for dmenu.
setOptions :: MonadDMenu m => m ()
setOptions = do
    DMenu.numLines        .= 0
    DMenu.caseInsensitive .= True
    DMenu.font            .= "Inconsolata Regular-10"
    DMenu.normalBGColor   .= HexColor 0x282A36
    DMenu.normalFGColor   .= HexColor 0xBBBBBB
    DMenu.selectedBGColor .= HexColor 0x8BE9FD
    DMenu.selectedFGColor .= HexColor 0x000000
