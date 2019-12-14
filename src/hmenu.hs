module Main
    ( main
    ) where

-- Local imports
import Core.Select
    ( decideSelection
    , formatUserPaths
    , getExecutables
    , makeNewEntries
    , selectWith
    , sortByValues
    , tryRead
    )
import Core.Toml
    ( Config(Config, dmenuExe, files, open)
    , getUserConfig
    , histFile
    , hmenuPath
    )

-- ByteString
import qualified Data.ByteString.Char8 as BS

-- Map
import qualified Data.Map.Strict as Map

-- Other imports
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getEnv)


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
    Config{ dmenuExe, files, open } <- getUserConfig

    -- See Note [Caching]
    -- Files the user added in the config file.
    home  <- BS.pack <$> getEnv "HOME"
    let userFiles = formatUserPaths home files

    -- Everything new as a map.
    -- 'mappend' for maps is the union (as expected).
    execs <- getExecutables
    let pathPlus = makeNewEntries $ userFiles <> execs

    -- New map where everything old (i.e. not in the $PATH or the config
    -- anymore) is thrown out and anything new is added to the map.
    hist <- tryRead =<< histFile
    let inters = hist `Map.intersection` pathPlus
        newMap = inters <> pathPlus

    -- Let the user select something from the list.
    selection <- selectWith opts (sortByValues newMap) dmenuExe

    -- Process output.
    case selection of
        Left  _ -> pure ()  -- silently fail
        Right s -> decideSelection s open newMap  -- Process output.

{- Note [Caching]
   ~~~~~~~~~~~~~~~~~~~~~~
   Doing the caching *after* the user has selected something may be
   better (in terms of perceived speed), though 'hmenu' would "lag
   behind" for one execution when things are updated.

   NOTE: This might be a non-issue as command line arguments for running
         with or without a cache update are planned anyways.
   NOTE: As of version 0.2.0 we're almost as fast as before being able to keep
         track of often used commands, so neither of this may be necessary.
-}
