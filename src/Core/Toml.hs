module Core.Toml
    ( Config(..)
    , getUserConfig
    , hmenuPath
    , histFile
    ) where

-- ByteString
import Data.ByteString (ByteString)

-- Text
import qualified Data.Text.IO as T

-- Tomland
import           Toml (TomlCodec, (.=))
import qualified Toml

-- Other imports
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import System.Directory (XdgDirectory(XdgConfig), doesFileExist, getXdgDirectory)
import System.FilePath ((</>))


-- ShowS for ByteString.
type ShowBS = ByteString -> ByteString

-- | Type that the parsed toml gets shoved into
data Config' = Config'
    { cfilePrefix :: !(Maybe ByteString)
    , cfiles      :: !(Maybe [ByteString])
    , copen       :: !(Maybe ByteString)
    , cdmenuExe   :: !(Maybe String)
    }

-- | Type we create from the parsed toml with certain default values in place on
-- 'Nothing'.
data Config = Config
    { filePrefix :: !ByteString
    , files      :: ![ByteString]
    , open       :: !ShowBS
    , dmenuExe   :: !String
    }

-- | Empty config type with all the default values.
emptyConfig :: Config
emptyConfig = Config
    { filePrefix = "file:"
    , files      = []
    , open       = ("xdg-open" <>)
    , dmenuExe   = "dmenu"
    }

-- | XDG_CONFIG
xdgConfig :: IO FilePath
xdgConfig = getXdgDirectory XdgConfig ""

-- | Path to the hmenu directory.
-- '~/.config/hmenu'
hmenuPath :: IO FilePath
hmenuPath = xdgConfig <&> (</> "hmenu")

-- | Path to the history file.
-- '~/.config/hmenu/histFile'
histFile :: IO FilePath
histFile = hmenuPath <&> (</> "histFile")

-- | Parse the toml.
configCodec :: TomlCodec Config'
configCodec = Config'
    <$> Toml.dioptional (Toml.byteString "file-prefix"        ) .= cfilePrefix
    <*> Toml.dioptional (Toml.arrayOf Toml._ByteString "files") .= cfiles
    <*> Toml.dioptional (Toml.byteString "open"               ) .= copen
    <*> Toml.dioptional (Toml.string "executable"             ) .= cdmenuExe

-- | Try to find a user config and, if it exists, parse it.
getUserConfig :: IO Config
getUserConfig = do
    -- Default path where to look for the config file.
    -- '~/.config/hmenu/hmenu.toml'
    cfgFile <- hmenuPath <&> (</> "hmenu.toml")

    -- If file doesn't exist we return a type with default values, otherwise we
    -- try to parse the config and see what's there.
    isFile <- doesFileExist cfgFile
    if not isFile
        then pure emptyConfig
        else do
            -- Read and evaluate file.
            tomlFile <- T.readFile cfgFile
            pure $ case Toml.decode configCodec tomlFile of
                -- If parsing failed just use default settings.
                Left  _   -> emptyConfig
                -- If no config, fill in default values.
                Right cfg -> makeConfig cfg

makeConfig :: Config' -> Config
makeConfig Config'{ cfilePrefix, cfiles, copen, cdmenuExe } =
    Config
        { filePrefix = fromMaybe defPrefix cfilePrefix
        , files      = fromMaybe defFiles  cfiles
        , dmenuExe   = fromMaybe defDmenu  cdmenuExe
        , open       = maybe defOpen (<>) copen
        }
  where
    defPrefix = filePrefix emptyConfig
    defOpen   = open       emptyConfig
    defFiles  = files      emptyConfig
    defDmenu  = dmenuExe   emptyConfig
