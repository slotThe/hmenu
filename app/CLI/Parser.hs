module CLI.Parser
    ( Options(..)
    , pOptions
    , parseArgs
    , options
    ) where

-- Other imports
import Options.Applicative
    ( Parser, ParserInfo, fullDesc, header, help, helper, info, long, metavar
    , short, strOption, value
    )


-- | Options the user may specify on the command line.
newtype Options = Options
    { historyPath :: FilePath
    }

-- | Parse all command line options.
pOptions :: Parser Options
pOptions = Options
    <$> pHistoryPath

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

-- | Create an info type from our options, adding help text and other nice
-- features.
options :: ParserInfo Options
options = info
    (helper <*> pOptions)  -- create "--help"
    (  header "hmenu - a small wrapper around dmenu"
    <> fullDesc
    )

-- | Separate dmenu's arguments from hmenu's arguments.
parseArgs
    :: [String]
    -> ([String], [String])  -- ^ (hmenu args, dmenu args)
parseArgs args =
    let (h, d) = span (/= "--") args
     in (h, drop 1 d)
