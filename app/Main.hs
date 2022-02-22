module Main
    ( main  -- :: IO ()
    ) where

import CLI.Parser (Options (Options, dmenuOpts, historyPath, onlyFiles), options)
import Core.Select (evalDirs, getExecutables, makeNewEntries, runUpdate, selectWith, sortByValues, tryRead)
import Core.Toml (Config (Config, dmenuExe, files, histPath), getUserConfig)
import Core.Util (histFile, hmenuPath, tryAddPrefix)

import Data.Map.Strict qualified as Map

import Options.Applicative (execParser)
import System.Posix.Env.ByteString (getEnvDefault)


-- | Execute dmenu and then do stuff.
main :: IO ()
main = do
    -- Get command line options and parse them.
    Options{ historyPath, dmenuOpts, onlyFiles } <- execParser options

    -- Create the `hmenu' directory (and all parents) if necessary and
    -- parse user config.
    createDirectoryIfMissing True =<< hmenuPath
    cfg@Config{ dmenuExe, files } <- getUserConfig

    -- Files the user added in the config file; see Note [Caching].
    home      <- getEnvDefault "HOME" ""
    userFiles <- evalDirs $ map (tryAddPrefix home) files

    -- See Note [Updating].
    newEntries <- makeNewEntries . (userFiles <>) <$> getExecutables
    hp         <- maybe histFile pure historyPath  -- path to history file
    hist       <- tryRead hp
    let newMap = (hist `Map.intersection` newEntries) <> newEntries

    -- Let the user select something from the list.
    let selectFrom | onlyFiles = filter (`elem` userFiles)
                   | otherwise = id
    selection <- selectWith dmenuOpts
                            (selectFrom $! sortByValues newMap)
                            dmenuExe

    -- Process output.
    case selection of
        Left  _ -> pure ()  -- silently fail
        Right s -> runUpdate s cfg{ files = userFiles, histPath = hp } newMap

{- Note [Updating]
   ~~~~~~~~~~~~~~~~~~~~~~
We create two new maps; one for all the "new things" (newEntries) and
one for everything "old" (i.e., known to us).  Then, what we are doing
(with A = 'hist' and B = 'newEntries') is this:

    (A ∩ B) ∪ B,

where the union and intersection for "Data.Map" maps is left-biased.
Note that, while in a set-theoretic context this would obviously equal
B, this is not the case here; the elements of A and B are tuples, but
the first component alone determines uniqueness!  We also explicitly
exploit the left-biasedness (I'm claiming that word) of the operations.

All in all, we add initialise everything new, throw everything out
that's not in the $PATH or in the config anymore, and update the rest.
-}

{- Note [Caching]
   ~~~~~~~~~~~~~~~~~~~~~~
Doing the caching *after* the user has selected something may be better
(in terms of perceived speed), though hmenu would "lag behind" for one
execution when things are updated.

As of version 0.2.0, we're almost as fast as before being able to keep
track of often used commands, so this is almost certainly a non-issue.
-}
