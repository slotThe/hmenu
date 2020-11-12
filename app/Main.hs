module Main
    ( main  -- :: IO ()
    ) where

import CLI.Parser (Options(Options, dmenuOpts, historyPath, onlyFiles), options)
import Core.Select
    ( evalDirs, getExecutables, makeNewEntries, runUpdate, selectWith
    , sortByValues, tryRead
    )
import Core.Toml (Config(Config, dmenuExe, files, histPath), getUserConfig)
import Core.Util (histFile, hmenuPath, tryAddPrefix)

import qualified Data.Map.Strict as Map

import Options.Applicative (execParser)
import System.Posix.Env.ByteString (getEnvDefault)


-- | Execute dmenu and then do stuff.
main :: IO ()
main = do
    -- Get command line options and parse them.
    Options{ historyPath, dmenuOpts, onlyFiles } <- execParser options

    -- Create the @hmenu@ directory (and all parents) if necessary.
    createDirectoryIfMissing True =<< hmenuPath

    -- Try to parse the config file (if it exists).
    cfg@Config{ dmenuExe, files } <- getUserConfig

    -- See Note [Caching]
    -- Files the user added in the config file.
    home      <- getEnvDefault "HOME" ""
    userFiles <- evalDirs $ map (tryAddPrefix home) files

    -- Everything new as a map.
    execsAndFiles <- makeNewEntries . (userFiles <>) <$> getExecutables

    -- Create a new map where everything old (i.e. not in the @$PATH@ or the
    -- config anymore) is thrown out and anything new is added to the map.
    hp   <- maybe histFile pure historyPath  -- Path to the history file.
    hist <- tryRead hp
    let newMap = (hist `Map.intersection` execsAndFiles) <> execsAndFiles
        -- 'mappend' for maps is the union (as expected).
        -- Note that intersection and union for maps is left-biased.

    -- Let the user select something from the list.
    let selectFrom | onlyFiles = filter (`elem` userFiles)
                   | otherwise = id
    selection <-
        selectWith dmenuOpts
                   (selectFrom $! sortByValues newMap)
                   dmenuExe

    -- Process output.
    case selection of
        Left  _ -> pure ()  -- silently fail
        Right s -> runUpdate s cfg{ files = userFiles, histPath = hp } newMap

{- Note [Caching]
   ~~~~~~~~~~~~~~~~~~~~~~
   Doing the caching *after* the user has selected something may be better (in
   terms of perceived speed), though hmenu would "lag behind" for one execution
   when things are updated.  As of version 0.2.0, we're almost as fast as before
   being able to keep track of often used commands, so this is almost certainly
   a non-issue.
-}
