module Core.Toml
    ( Config(..)
    , getUserConfig
    , hmenuPath
    , histFile
    ) where

-- Local imports
import Core.Util (ShowBS)

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


-- | Type we create from the parsed toml with certain default values in place on
-- 'Nothing'.
data Config = Config
    { files    :: ![ByteString]
    , open     :: !ShowBS
    , dmenuExe :: !String
    }

-- | Empty config type with all the default values.
defaultCfg :: Config
defaultCfg = Config
    { files    = []
    , open     = ("xdg-open" <>)
    , dmenuExe = "dmenu"
    }

-- | Type that the parsed toml gets shoved into
data Config' = Config'
    { cfiles    :: !(Maybe [ByteString])
    , copen     :: !(Maybe ByteString)
    , cdmenuExe :: !(Maybe String)
    }

-- | XDG_CONFIG_HOME
xdgConfig :: IO FilePath
xdgConfig = getXdgDirectory XdgConfig ""

-- | Path to the hmenu directory.
-- 'XDG_CONFIG_HOME\/hmenu', so probably '~\/.config\/hmenu'.
hmenuPath :: IO FilePath
hmenuPath = xdgConfig <&> (</> "hmenu")

-- | Path to the history file.
-- '~/.config/hmenu/histFile'
histFile :: IO FilePath
histFile = hmenuPath <&> (</> "histFile")

-- | Parse the config file.
configCodec :: TomlCodec Config'
configCodec = Config'
    <$> Toml.dioptional (Toml.arrayOf Toml._ByteString "files") .= cfiles
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
        then pure defaultCfg
        else do
            -- Read and evaluate file.
            tomlFile <- T.readFile cfgFile
            pure $ case Toml.decode configCodec tomlFile of
                Left  _   -> defaultCfg
                Right cfg -> makeConfig cfg

-- | Build up a config based on what the parser could find, substitute in
-- default values for fields that were not able to parse/missing.
makeConfig :: Config' -> Config
makeConfig Config'{ cfiles, copen, cdmenuExe } =
    Config
        { files    = fromMaybe defFiles cfiles
        , dmenuExe = fromMaybe defDmenu cdmenuExe
        , open     = maybe defOpen mappend copen
        }
  where
    defOpen  = open     defaultCfg
    defFiles = files    defaultCfg
    defDmenu = dmenuExe defaultCfg
