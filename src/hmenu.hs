{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

-- Local imports
import           Core.Util                      ( clean
                                                , addPrefix
                                                , splitOnColon
                                                )

-- DMenu
import           DMenu                          ( MonadDMenu
                                                , Color(..)
                                                , (.=)
                                                )
import qualified DMenu

-- Other imports
import           Control.Monad                  ( void )
import           Data.Containers.ListUtils      ( nubOrd )
import           Data.List                      ( isPrefixOf
                                                , sort
                                                )
import           System.Directory               ( listDirectory )
import           System.Environment             ( getEnv )
import           System.Process                 ( spawnCommand )


-- | We are mathematicians, we like this sort of thing.
type ShowSL = [String] -> [String]

-- | `$PATH`
path :: IO String
path = getEnv "PATH"

-- | `$HOME`
userHome :: IO String
userHome = (++ "/") <$> getEnv "HOME"

-- | Add user defined configuration to *something* (this will be the stuff
-- extracted from path).
addConfigWith :: String -> ShowSL
addConfigWith = (++) . userCfgs

-- | Add the default prefix for config files.
-- Defaults to "cfg".
addCfg :: ShowS
addCfg = ("cfg:" ++)

-- | User defined config files.
userCfgs :: String -> [String]
userCfgs home = map
    (addCfg . addPrefix home)
    [ ".zshrc"
    , ".muttrc"
    , ".emacs.d/configuration.org"
    , ".emacs.d/init.el"
    , ".vimrc"
    , ".xmonad/src/xmonad.hs"
    , ".config/xmobarrc/src/xmobarrc.hs"
    , ".config/topgrade.toml"
    , ".config/zsh/completion.zsh"
    , ".config/zsh/keybindings.zsh"
    , ".config/zsh/aliases.zsh"
    , ".config/zsh/aliases-sensitive.zsh"
    ]

-- | Get all executables from all dirs in `$PATH`.
getExecutables :: IO [String]
getExecutables = do
    home     <- userHome
    pathDirs <- splitOnColon <$> path
    dirs     <- mconcat $ map listDirectory pathDirs
    return $ addConfigWith home dirs

-- | User set opening script, defaults to using 'xdg-open'.
open :: ShowS
open = ("decide-link.sh " ++)

-- | spawn a command and forget about it.
spawn :: String -> IO ()
spawn = void . spawnCommand

-- | execute dmenu and then do stuff.
main :: IO ()
main = do
    e <- getExecutables
    -- remove duplicates, then sort.
    let exes = sort . nubOrd $ e
    -- Let the user select something from the list.
    selection <- DMenu.select setOptions exes

    case selection of
        Left  _ -> return () -- silently fail
        Right s -> if
            | "cfg:" `isPrefixOf` s -> spawn . open . clean $ s
            | otherwise             -> spawn s

-- | Options for dmenu.
setOptions :: MonadDMenu m => m ()
setOptions = do
    DMenu.numLines .= 0
    DMenu.caseInsensitive .= True
    DMenu.font .= "Inconsolata Regular-10"
    DMenu.normalBGColor .= HexColor 0x282A36
    DMenu.normalFGColor .= HexColor 0xBBBBBB
    DMenu.selectedBGColor .= HexColor 0x8BE9FD
    DMenu.selectedFGColor .= HexColor 0x000000
