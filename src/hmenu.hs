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
    ( Config(Config)
    , dmenuExe
    , filePrefix
    , files
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


-- | Execute dmenu and then do stuff.
main :: IO ()
main = do
    -- Command line arguments, these get passed straight to dmenu.
    opts <- getArgs

    -- Create the 'hmenu' directory (and all parents) if necessary.
    createDirectoryIfMissing True =<< hmenuPath

    -- Try to parse the config file (if it exists).
    cfg@Config{ dmenuExe = dmenu, filePrefix, files } <- getUserConfig


    -- See Note [Caching]
    -- Files the user added in the config file.
    home  <- BS.pack <$> getEnv "HOME"
    let uFiles = formatUserPaths home filePrefix files

    -- Everything new as a map.
    execs <- getExecutables
    let newFiles = makeNewEntries (uFiles <> execs)

    -- New map where everything old (i.e. not in the PATH or the config anymore)
    -- is thrown out and anything new is added to the map.
    file <- tryRead =<< histFile
    let inters = file `Map.intersection` newFiles
    let newMap = inters <> newFiles

    -- Let the user select something from the list.
    selection <- selectWith opts (sortByValues newMap) dmenu

    -- Process output.
    case selection of
        Left  _ -> pure ()  -- silently fail
        Right s -> decideSelection s cfg newMap  -- Process output.

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
