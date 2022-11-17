module CLI.Parser
    ( Options(..)
    , pOptions     -- :: Parser Options
    , options      -- :: ParserInfo Options
    ) where

import Core.Util
import Options.Applicative (Parser, ParserInfo, argument, auto, fullDesc, header, help, helper, info, long, metavar, option, short, str, strOption, switch, value)


-- | Options the user may specify on the command line.
type Options :: Type
data Options = Options
    { historyPath :: Maybe FilePath
    , onlyFiles   :: Bool      -- ^ __Only__ show files
    , decay       :: Double    -- ^ Decay to multiple not-selected-items with
    , dmenuOpts   :: [String]  -- ^ Positional arguments
    }

-- | Parse all command line options.
pOptions :: Parser Options
pOptions = Options <$> pHistoryPath <*> pOnlyFiles <*> pDecay <*> pDmenuOpts

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

pOnlyFiles :: Parser Bool
pOnlyFiles = switch
     ( long "files-only"
    <> short 'o'
    <> help "Whether to only show the user-specified files."
     )

pDecay :: Parser Double
pDecay = option auto
     ( long "decay"
    <> short 'd'
    <> help "Decay to multiply not-selected items with."
    <> metavar "D"
    <> value 1
     )

-- | Options to get passed straight to dmenu.
pDmenuOpts :: Parser [String]
pDmenuOpts = many $ argument str (metavar "-- DMENU_OPTS")

-- | Create an info type from our options, adding help text and other
-- nice features.
options :: ParserInfo Options
options = info
    (helper <*> pOptions)  -- create "--help"
    (  header "hmenu - a small wrapper around dmenu"
    <> fullDesc
    )
