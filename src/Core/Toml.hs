module Core.Toml
    ( Config(..)
    , getUserConfig
    ) where

-- Text
import qualified Data.Text.IO as T

-- Tomland
import           Toml (TomlCodec, (.=))
import qualified Toml

-- Other imports
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import System.Directory (XdgDirectory(XdgConfig), doesFileExist, getXdgDirectory)


-- | Type that the parsed toml gets shoved into
data Config' = Config'
    { cfilePrefix :: !(Maybe String)
    , cfiles      :: !(Maybe [String])
    , copen       :: !(Maybe String)
    , cdmenuExe   :: !(Maybe String)
    }

-- | Type we create from the parsed toml with certain default values in place on
-- 'Nothing'.
data Config = Config
    { filePrefix :: !String
    , files      :: ![String]
    , open       :: !ShowS
    , dmenuExe   :: !String
    }

-- | Empty config type with all the default values.
emptyConfig :: Config
emptyConfig = Config
    { filePrefix = "file:"
    , files      = []
    , open       = ("xdg-open" ++)
    , dmenuExe   = "dmenu"
    }

-- | Convenience
io :: MonadIO m => IO a -> m a
io = liftIO

-- | XDG_CONFIG
xdgConfig :: MonadIO m => m FilePath
xdgConfig = io $ getXdgDirectory XdgConfig ""

-- | Parse the toml.
configCodec :: TomlCodec Config'
configCodec = Config'
    <$> Toml.dioptional (Toml.string "file-prefix"        ) .= cfilePrefix
    <*> Toml.dioptional (Toml.arrayOf Toml._String "files") .= cfiles
    <*> Toml.dioptional (Toml.string "open"               ) .= copen
    <*> Toml.dioptional (Toml.string "executable"         ) .= cdmenuExe

-- | Try to find a user config and, if it exists, parse it.
getUserConfig :: IO Config
getUserConfig = do
    -- Default path where to look for the config file.
    -- ~/.config/hmenu.toml
    xdgConfDir <- xdgConfig
    let cfgFile = xdgConfDir ++ "/hmenu.toml"

    -- If file doesn't exist we return a type with default values, otherwise we
    -- try to parse the config and see what's there.
    isFile <- io $ doesFileExist cfgFile
    if not isFile
        then pure emptyConfig
        -- Read and evaluate file.
        else do
            tomlFile <- io $ T.readFile cfgFile
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
        , open       = maybe defOpen (++) copen
        }
  where
    defPrefix = filePrefix emptyConfig
    defOpen   = open       emptyConfig
    defFiles  = files      emptyConfig
    defDmenu  = dmenuExe   emptyConfig
