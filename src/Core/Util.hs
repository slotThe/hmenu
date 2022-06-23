module Core.Util
    ( -- * Better prelude
      module BasePrelude
    , ByteString
    , Map
    , XdgDirectory(XdgConfig)
    , ifM
    , doesFileExist
    , proc
    , getXdgDirectory
    , spawnCommand
    , createDirectoryIfMissing

      -- * Types
    , ShowBS              -- type alias: ByteString -> ByteString
    , OpenIn(Term, Open)
    , Items               -- type alias: Map ByteString Int

      -- * Combining file paths
    , tryAddPrefix        -- :: ByteString -> ByteString -> ByteString
    , (<</>>)             -- :: Functor f => f FilePath -> FilePath -> f FilePath
    , (</>)               -- :: FilePath -> FilePath -> FilePath

      -- * System file paths
    , hmenuPath           -- :: IO FilePath
    , histFile            -- :: IO FilePath

      -- * Running commands
    , spawn               -- :: ByteString -> IO ()
    , openWith            -- :: OpenIn -> ShowBS -> ByteString -> ByteString
    ) where

import BasePrelude
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.UTF8  (toString)
import Data.Map.Strict (Map)
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Posix.FilePath qualified as BS -- used for ByteString version of </>
import System.Process (proc, spawnCommand)

-- | Like 'if', but in a monadic context.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y
{-# INLINE ifM #-}

-- | Type for an Map that describes all of the executables with their
-- ratings.
type Items = Map ByteString Double

-- | Type for helping to decide how to open something.
data OpenIn = Term ShowBS | Open ShowBS

-- | 'ShowS' for 'ByteString' because it is shorter :>.
type ShowBS = ByteString -> ByteString

{- | Add a prefix to a string if the string is not starting with "/".

This will ensure the user can specify absolute paths to files, but also
conveniently use relative paths (starting from @$HOME@) if that is
desired.
-}
tryAddPrefix :: ByteString -> ByteString -> ByteString
tryAddPrefix prefix xs
    | BS.null xs  = ""
    | isSpecial x = xs
    | otherwise   = prefix BS.</> xs
  where
    x         :: Char         = BS.head xs
    isSpecial :: Char -> Bool = (`elem` ['/', '~'])

-- | Spawn a command and forget about it.
spawn :: ByteString -> IO ()
spawn = void . spawnCommand . toString

-- | Open something.
openWith
    :: OpenIn      -- ^ How (and with what) to open something.
    -> ByteString  -- ^ The thing to open.
    -> ByteString
openWith = \case
    Term t -> t . (" -e " <>) . escape
    Open o -> o . (" "    <>) . escape
  where
    escape :: ByteString -> ByteString
    escape = ("\"" <>) . (<> "\"") . BS.concatMap \case
        '('  -> "\\("
        ')'  -> "\\)"
        '\'' -> "'\"'\"'"
        '\"' -> "\\\""
        x    -> BS.singleton x

-- | XDG_CONFIG_HOME
xdgConfig :: IO FilePath
xdgConfig = getXdgDirectory XdgConfig ""

-- | Path to the hmenu directory.
-- @XDG_CONFIG_HOME\/hmenu@, so probably @~\/.config\/hmenu@.
hmenuPath :: IO FilePath
hmenuPath = xdgConfig <</>> "hmenu"

-- | Path to the history file.
-- @~\/.config\/hmenu\/histFile@
histFile :: IO FilePath
histFile = hmenuPath <</>> "histFile"

-- | Functorial path-append operation.
infixr 5 <</>>  -- same as </>
(<</>>) :: Functor f => f FilePath -> FilePath -> f FilePath
liftedFp <</>> fp = liftedFp <&> (</> fp)

-- | Combine two paths into a new path.
-- Adapted from: <https://hackage.haskell.org/package/filepath>
infixr 5 </>
(</>) :: FilePath -> FilePath -> FilePath
(</>) a b
    | Just '/' == listToMaybe b = b  -- leading path separator
    | otherwise = combineAlways a b
  where
    -- Combine two paths, assuming rhs is NOT absolute.
    combineAlways :: FilePath -> FilePath -> FilePath
    combineAlways z w
        | null z        = w
        | null w        = z
        | last z == '/' = z ++ w
        | otherwise     = z ++ "/" ++ w
