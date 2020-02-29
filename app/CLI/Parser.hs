module CLI.Parser
    ( Options(..)
    , pOptions
    , options
    ) where

-- Other imports
import Options.Applicative
    ( Parser, ParserInfo, argument, fullDesc, header, help, helper, info, long
    , many, metavar, short, str, strOption, value
    )


-- | Options the user may specify on the command line.
data Options = Options
    { historyPath :: FilePath
    , dmenuOpts   :: [String]
    }

-- | Parse all command line options.
pOptions :: Parser Options
pOptions = Options
    <$> pHistoryPath
    <*> pDmenuOpts

-- | Parse the 'historyPath' option.  Basically the user may specify an
-- alternative history file to use.
pHistoryPath :: Parser FilePath
pHistoryPath = strOption
     ( long "histfile"
    <> short 'f'
    <> metavar "FILE"
    <> help "Manually set path to history file."
    <> value ""  -- This will actually default to
                 -- "$XDG_CONFIG_HOME/hmenu/histFile" or an equivalent.
     )

-- | Options to get passed straight to dmenu.
pDmenuOpts :: Parser [String]
pDmenuOpts = many $ argument str (metavar "-- DMENU_OPTS")

-- | Create an info type from our options, adding help text and other nice
-- features.
options :: ParserInfo Options
options = info
    (helper <*> pOptions)  -- create "--help"
    (  header "hmenu - a small wrapper around dmenu"
    <> fullDesc
    )
