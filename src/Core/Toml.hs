module Core.Toml
    ( Config(..)
    , getUserConfig
    ) where

-- DMenu
import DMenu (Color(..))

-- Tomland
import           Toml (TomlCodec, (.=))
import qualified Toml

-- Text
import qualified Data.Text.IO as T

-- Other imports
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import System.Directory
    ( XdgDirectory(XdgConfig)
    , doesFileExist
    , getXdgDirectory
    )

-- | Type that the parsed toml gets shoved into
data Config' = Config'
    { cfilePrefix :: !(Maybe String)
    , cfiles      :: !(Maybe [String])
    , copen       :: !(Maybe String)
    , cNumLines   :: !(Maybe Int)
    , ccaseIns    :: !(Maybe Bool)
    , cFont       :: !(Maybe String)
    , cnormBgCol  :: !(Maybe Int)
    , cnormFgCol  :: !(Maybe Int)
    , cselBgCol   :: !(Maybe Int)
    , cselFgCol   :: !(Maybe Int)
    }

-- | Type we create from the parsed toml with certain default values in place on
-- 'Nothing'.
data Config = Config
    { filePrefix :: !String
    , files      :: ![String]
    , open       :: !ShowS
    , numLines   :: !Int
    , caseIns    :: !Bool
    , font       :: !String
    , normBgCol  :: !Color
    , normFgCol  :: !Color
    , selBgCol   :: !Color
    , selFgCol   :: !Color
    }

-- | Convenience
io :: MonadIO m => IO a -> m a
io = liftIO

-- | XDG_CONFIG
xdgConfig :: MonadIO m => m FilePath
xdgConfig = io $ getXdgDirectory XdgConfig ""

-- | Empty config type with all the default values.
-- TODO I should probably use dmenu's default values here instead of my own.
emptyConfig :: Config
emptyConfig = Config
    { filePrefix = "file:"
    , files      = []
    , open       = ("xdg-open" ++)
    , numLines   = 0
    , caseIns    = True
    , font       = "Inconsolata Regular-10"
    , normBgCol  = HexColor 0x282A36
    , normFgCol  = HexColor 0xBBBBBB
    , selBgCol   = HexColor 0x8BE9FD
    , selFgCol   = HexColor 0x000000
    }

-- | Parse the toml.
configCodec :: TomlCodec Config'
configCodec = Config'
    <$> Toml.dioptional (Toml.string "file-prefix"        ) .= cfilePrefix
    <*> Toml.dioptional (Toml.arrayOf Toml._String "files") .= cfiles
    <*> Toml.dioptional (Toml.string "open"               ) .= copen
    <*> Toml.dioptional (Toml.int "num-lines"             ) .= cNumLines
    <*> Toml.dioptional (Toml.bool "case-insensitive"     ) .= ccaseIns
    <*> Toml.dioptional (Toml.string "font"               ) .= cFont
    <*> Toml.dioptional (Toml.int "bg-color"              ) .= cnormBgCol
    <*> Toml.dioptional (Toml.int "fg-color"              ) .= cnormFgCol
    <*> Toml.dioptional (Toml.int "selected-bg-color"     ) .= cselBgCol
    <*> Toml.dioptional (Toml.int "selected-fg-color"     ) .= cselFgCol

-- | Try to find a user config and, if it exists, parse it.
-- | TODO: There is probably a better solution for this
getUserConfig :: MonadIO m => m Config
getUserConfig = do
    -- Default path where to look for the config file.
    -- ~/.config/hmenu.toml
    xdgConfDir <- xdgConfig
    let cfgFile = xdgConfDir ++ "/hmenu.toml"

    -- If file doesn't exist we return a type with default values, otherwise we
    -- try to parse the config and see what's there.
    isFile <- io $ doesFileExist cfgFile
    if not isFile
        then return emptyConfig
        -- Read and evaluate file.
        else do
            tomlFile <- io $ T.readFile cfgFile
            case Toml.decode configCodec tomlFile of
                -- If parsing failed just use default settings.
                Left _ -> return emptyConfig
                -- If no config, fill in default values.
                Right Config' { cfilePrefix
                              , cfiles
                              , copen
                              , cNumLines
                              , ccaseIns
                              , cFont
                              , cnormBgCol
                              , cnormFgCol
                              , cselBgCol
                              , cselFgCol
                              } ->
                    return Config
                        { filePrefix = fromMaybe defPrefix cfilePrefix
                        , files      = fromMaybe defFiles  cfiles
                        , numLines   = fromMaybe defLines  cNumLines
                        , caseIns    = fromMaybe defCase   ccaseIns
                        , font       = fromMaybe defFont   cFont
                        , normBgCol  = maybe defBgCol    HexColor cnormBgCol
                        , normFgCol  = maybe defFgCol    HexColor cnormFgCol
                        , selBgCol   = maybe defSelBgCol HexColor cselBgCol
                        , selFgCol   = maybe defSelFgCol HexColor cselFgCol
                        , open       = maybe defOpen     (++)     copen
                        }
  where
    defPrefix   = filePrefix emptyConfig
    defOpen     = open emptyConfig
    defFiles    = files emptyConfig
    defLines    = numLines emptyConfig
    defCase     = caseIns emptyConfig
    defFont     = font emptyConfig
    defBgCol    = normBgCol emptyConfig
    defFgCol    = normFgCol emptyConfig
    defSelBgCol = selBgCol emptyConfig
    defSelFgCol = selFgCol emptyConfig
