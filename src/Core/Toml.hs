module Core.Toml
    ( Config(..)
    , getUserConfig
    ) where

-- Tomland
import           Toml (TomlCodec, (.=))
import qualified Toml

-- Text
import qualified Data.Text.IO as T

-- Other imports
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
    }

-- | Type we create from the parsed toml with certain default values in place on
-- 'Nothing'.
data Config = Config
    { filePrefix :: !String
    , files      :: ![String]
    , open       :: !String
    }

-- | XDG_CONFIG
xdgConfig :: IO FilePath
xdgConfig = getXdgDirectory XdgConfig ""

-- | Empty config type with all the default values.
emptyConfig :: Config
emptyConfig = Config "file:" [] "xdg-open"

-- | Parse the toml.
configCodec :: TomlCodec Config'
configCodec = Config'
    <$> Toml.dioptional (Toml.string "file-prefix")         .= cfilePrefix
    <*> Toml.dioptional (Toml.arrayOf Toml._String "files") .= cfiles
    <*> Toml.dioptional (Toml.string "open")                .= copen

-- | Try to find a user config and, if it exists, parse it.
-- | TODO: There is probably a better solution for this
getUserConfig :: IO Config
getUserConfig = do
    xdgConfDir <- xdgConfig

    -- Default path where to look for the config file.
    -- ~/.config/hmenu.toml
    let cfgFile = xdgConfDir ++ "/hmenu.toml"

    -- If file doesn't exist we return a type with default values, otherwise we
    -- try to parse the config and see what's there.
    isFile <- doesFileExist cfgFile
    if not isFile
        then return emptyConfig
        else do
            -- Read and evaluate file.
            tomlFile <- T.readFile cfgFile
            case Toml.decode configCodec tomlFile of
                -- If parsing failed just use default settings.
                Left _ -> return emptyConfig
                -- If no config, fill in default values.
                Right Config'{ cfilePrefix, cfiles, copen } -> return Config
                    { filePrefix = fromMaybe defPrefix cfilePrefix
                    , files      = fromMaybe defFiles  cfiles
                    , open       = fromMaybe defOpen   copen
                    }
  where
    defPrefix = filePrefix emptyConfig
    defOpen   = open       emptyConfig
    defFiles  = files      emptyConfig
