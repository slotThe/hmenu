module Core.Toml
    ( Config(..)
    , getUserConfig  -- :: IO Config
    ) where

import Core.Util

import Data.Text.IO qualified as T
import Toml         qualified

import Toml (Codec (Codec, codecRead), TomlCodec, (.=), (<!>))


-- | Hdmenu's config file.
type Config :: Type
data Config = Config
    { files    :: [ByteString]
    , open     :: ShowBS
    , dmenuExe :: FilePath
    , term     :: ShowBS
    , tty      :: [ByteString]
    , decay    :: Double
    , histPath :: FilePath
      -- ^ Command line option, NOT specifiable in the config file.
    }

-- | Empty config type with all the default values.
defaultCfg :: Config
defaultCfg = Config
    { files    = []
    , dmenuExe = "dmenu"
    , open     = ("xdg-open" <>)
    , term     = ("xterm"    <>)
    , tty      = []
    , decay    = 1
    , histPath = ""
    }

-- | Parse the config file.
configCodec :: TomlCodec Config
configCodec = Config
    <$> defFiles (Toml.arrayOf Toml._ByteString "files")           .= files
    <*> toDL (defOpen (Toml.byteString "open"))                    .= open
    <*> defDmenu (Toml.string "executable")                        .= dmenuExe
    <*> toDL (defTerm (Toml.byteString "terminal"))                .= term
    <*> defTtyProgs (Toml.arrayOf Toml._ByteString "tty-programs") .= tty
    <*> defDecay (Toml.double "decay")                             .= decay
    <*> pure ""
  where
    -- Parse an option or—in case it's missing—return a default value.
    tomlWithDefault :: a -> TomlCodec a -> TomlCodec a
    tomlWithDefault def codec@Codec{ codecRead } =
        codec { codecRead = codecRead <!> const (pure def) }

    toDL :: TomlCodec ByteString -> TomlCodec ShowBS
        = Toml.dimap ($ "") (<>)

    defFiles    = tomlWithDefault (files    defaultCfg)
    defOpen     = tomlWithDefault (open     defaultCfg "")
    defDmenu    = tomlWithDefault (dmenuExe defaultCfg)
    defTerm     = tomlWithDefault (term     defaultCfg "")
    defTtyProgs = tomlWithDefault (tty      defaultCfg)
    defDecay    = tomlWithDefault (decay    defaultCfg)

-- | Try to find a user config and, if it exists, parse it.
getUserConfig :: IO Config
getUserConfig = do
    -- Default path where to look for the config file:
    --          ~/.config/hdmenu/hdmenu.toml
    cfgFile <- hdmenuPath <</>> "hdmenu.toml"
    ifM (doesFileExist cfgFile)
        (fromRight defaultCfg . Toml.decode configCodec <$> T.readFile cfgFile)
        (pure defaultCfg)
