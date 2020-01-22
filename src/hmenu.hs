module Main
    ( main
    ) where

-- Local imports
import Core.Select
    ( evalDirs
    , formatUserPaths
    , getExecutables
    , makeNewEntries
    , runUpdate
    , selectWith
    , sortByValues
    , tryRead
    )
import Core.Toml (Config(Config, dmenuExe, files), getUserConfig)
import Core.Util (histFile, hmenuPath)

-- Map
import qualified Data.Map.Strict as Map

-- Other imports
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Posix.Env.ByteString (getEnvDefault)


{- | Execute dmenu and then do stuff.
   NOTE: Intersection and union for maps are both left biased and this fact is
         used the calculations below.
-}
main :: IO ()
main = do
    -- Command line arguments, these get passed straight to dmenu.
    opts <- getArgs

    -- Create the 'hmenu' directory (and all parents) if necessary.
    createDirectoryIfMissing True =<< hmenuPath

    -- Try to parse the config file (if it exists).
    cfg@Config{ dmenuExe, files } <- getUserConfig

    -- See Note [Caching]
    -- Files the user added in the config file.
    home  <- getEnvDefault "HOME" ""
    userFiles <- evalDirs $ formatUserPaths home files
    let cfg' = cfg { files = userFiles }  -- gets passed to runUpdate

    -- Everything new as a map.
    execs <- getExecutables
    let pathPlus = makeNewEntries $ userFiles <> execs

    -- New map where everything old (i.e. not in the $PATH or the config
    -- anymore) is thrown out and anything new is added to the map.
    hist <- tryRead =<< histFile
    let inters = hist `Map.intersection` pathPlus
        newMap = inters <> pathPlus
        -- 'mappend' for maps is the union (as expected).

    -- Let the user select something from the list.
    selection <- selectWith opts (sortByValues newMap) dmenuExe

    -- Process output.
    case selection of
        Left  _ -> pure ()  -- silently fail
        Right s -> runUpdate s cfg' newMap

{- Note [Caching]
   ~~~~~~~~~~~~~~~~~~~~~~
   Doing the caching *after* the user has selected something may be better (in
   terms of perceived speed), though hmenu would "lag behind" for one execution
   when things are updated.  As of version 0.2.0, we're almost as fast as before
   being able to keep track of often used commands, so this is almost certainly
   a non-issue.
-}
