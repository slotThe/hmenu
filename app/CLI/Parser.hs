module CLI.Parser
    ( Options(..)
    , pOptions     -- :: Parser Options
    , options      -- :: ParserInfo Options
    ) where

import Options.Applicative (Parser, ParserInfo, argument, fullDesc, header, help, helper, info, long, metavar, short, str, strOption, switch)


-- | Options the user may specify on the command line.
data Options = Options
    { historyPath :: (Maybe FilePath)
    , onlyFiles   :: Bool      -- ^ __Only__ show files
    , dmenuOpts   :: [String]  -- ^ Positional arguments
    }

-- | Parse all command line options.
pOptions :: Parser Options
pOptions = Options <$> pHistoryPath <*> pOnlyFiles <*> pDmenuOpts

{- | Parse the 'historyPath' option, allowing the user to specify an
alternative history file to use.

If this is @Nothing@, the path will default to something like
"$XDG_CONFIG_HOME\/hmenu\/histFile".
-}
pHistoryPath :: Parser (Maybe FilePath)
pHistoryPath = optional $ strOption
     ( long "histfile"
    <> short 'f'
    <> metavar "FILE"
    <> help "Manually set path to history file."
     )

-- | Options to get passed straight to dmenu.
pDmenuOpts :: Parser [String]
pDmenuOpts = many $ argument str (metavar "-- DMENU_OPTS")

pOnlyFiles :: Parser Bool
pOnlyFiles = switch
     ( long "files-only"
    <> short 'o'
    <> help "Whether to only show the user-specified files."
     )

-- | Create an info type from our options, adding help text and other
-- nice features.
options :: ParserInfo Options
options = info
    (helper <*> pOptions)  -- create "--help"
    (  header "hmenu - a small wrapper around dmenu"
    <> fullDesc
    )
