module Core.Toml
    ( Config(..)
    , getUserConfig
    ) where

-- Local imports
import Core.Util (ShowBS, (</>), hmenuPath)

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
import System.Directory (doesFileExist)


-- | Type we create from the parsed toml with certain default values in place on
-- 'Nothing'.
data Config = Config
    { files    :: ![ByteString]
    , open     :: !ShowBS
    , dmenuExe :: !FilePath
    , term     :: !ShowBS
    , tty      :: ![ByteString]
    , histPath :: !FilePath      -- ^ Command line option, NOT specifiable in the
                                 -- config file.
    }

-- | Empty config type with all the default values.
defaultCfg :: Config
defaultCfg = Config
    { files    = []
    , dmenuExe = "dmenu"
    , open     = ("xdg-open" <>)
    , term     = ("xterm"    <>)
    , tty      = []
    , histPath = ""
    }

-- | Type that the parsed toml gets shoved into
data Config' = Config'
    { cfiles    :: !(Maybe [ByteString])
    , copen     :: !(Maybe ByteString)
    , cdmenuExe :: !(Maybe String)
    , cterm     :: !(Maybe ByteString)
    , ctty      :: !(Maybe [ByteString])
    }

-- | Parse the config file.
configCodec :: TomlCodec Config'
configCodec = Config'
    <$> Toml.dioptional (Toml.arrayOf Toml._ByteString "files") .= cfiles
    <*> Toml.dioptional (Toml.byteString "open"               ) .= copen
    <*> Toml.dioptional (Toml.string     "executable"         ) .= cdmenuExe
    <*> Toml.dioptional (Toml.byteString "terminal"           ) .= cterm
    <*> Toml.dioptional (Toml.arrayOf Toml._ByteString "tty-programs") .= ctty

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
makeConfig Config'{ cfiles, copen, cdmenuExe, ctty, cterm } =
    Config
        { files    = fromMaybe defFiles cfiles
        , dmenuExe = fromMaybe defDmenu cdmenuExe
        , tty      = fromMaybe defTty   ctty
        , open     = maybe defOpen mappend copen
        , term     = maybe defEmu  mappend cterm
        , histPath = ""
        }
  where
    defOpen  = open     defaultCfg
    defFiles = files    defaultCfg
    defDmenu = dmenuExe defaultCfg
    defEmu   = term     defaultCfg
    defTty   = tty      defaultCfg
