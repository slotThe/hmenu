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
import Data.Maybe
import System.Directory
    ( XdgDirectory(XdgConfig)
    , doesFileExist
    , getXdgDirectory
    )


-- | XDG_CONFIG
xdgConfig :: IO FilePath
xdgConfig = getXdgDirectory XdgConfig ""

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

-- | Empty config type with all the default values.
emptyConfig :: Config
emptyConfig = Config "cfg:" [] "xdg-open"

-- | Parse the toml.
configCodec :: TomlCodec Config'
configCodec = Config'
    <$> Toml.dioptional (Toml.string "file-prefix")         .= cfilePrefix
    <*> Toml.dioptional (Toml.arrayOf Toml._String "files") .= cfiles
    <*> Toml.dioptional (Toml.string "open")                .= copen

-- | Try to find a user config and, if it exists, parse it.
-- | TODO: Rewrite this
getUserConfig :: IO Config
getUserConfig = do
    xdgConfDir <- xdgConfig
    let cfgFile = xdgConfDir ++ "/hmenu.toml"
    isFile <- doesFileExist cfgFile
    -- If file doesn't exist we return an empty config.
    if not isFile
        then return emptyConfig
        else do
            -- Read and evaluate file
            tomlFile <- T.readFile cfgFile
            let res = Toml.decode configCodec tomlFile
            case res of
                -- If parsing failed just use default settings.
                Left _ -> return emptyConfig
                -- If no config, fill in default values.
                Right Config'{ cfilePrefix, cfiles, copen } -> return Config
                    { filePrefix = fromMaybe defPrefix cfilePrefix
                    , files      = fromMaybe []        cfiles
                    , open       = fromMaybe defOpen   copen
                    }
  where
    defPrefix = filePrefix emptyConfig
    defOpen   = open       emptyConfig
