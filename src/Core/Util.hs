module Core.Util
    ( -- * Types
      ShowBS              -- type alias: ByteString -> ByteString
    , OpenIn(Term, Open)
    , Items               -- type alias: Map ByteString Int

      -- * Combining file paths
    , tryAddPrefix        -- :: ByteString -> ByteString -> ByteString
    , (<<>>)              -- :: Functor f => f FilePath -> FilePath -> f FilePath
    , (</>)               -- :: FilePath -> FilePath -> FilePath

      -- * System file paths
    , hmenuPath           -- :: IO FilePath
    , histFile            -- :: IO FilePath

      -- * Running commands
    , spawn               -- :: ByteString -> IO ()
    , openWith            -- :: OpenIn -> ShowBS -> ByteString -> ByteString
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified System.Posix.FilePath as BS -- used for ByteString version of </>


-- | Type for an Map that describes all of the executables with their ratings.
type Items = Map ByteString Int

-- | Type for helping to decide how to open something.
data OpenIn = Term | Open

-- | ShowS for ByteString because it is shorter :>.
type ShowBS = ByteString -> ByteString

{- | Add a prefix to a string if the string is not starting with "/".
   This will ensure the user can specify absolute paths to files, but also
   conveniently use relative paths (starting from '$HOME') if that is desired.
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
spawn = void . spawnCommand . BS.unpack

-- | Open something.
openWith
    :: OpenIn      -- ^ Decide what to add to the opening function.
    -> ShowBS      -- ^ How to open something.
    -> ByteString  -- ^ The thing to open.
    -> ByteString
openWith Term t = t . (" -e " <>)
openWith Open o = o . (" "    <>)

-- | XDG_CONFIG_HOME
xdgConfig :: IO FilePath
xdgConfig = getXdgDirectory XdgConfig ""

-- | Path to the hmenu directory.
-- @XDG_CONFIG_HOME\/hmenu@, so probably @~\/.config\/hmenu@.
hmenuPath :: IO FilePath
hmenuPath = xdgConfig <<>> "hmenu"

-- | Path to the history file.
-- @~\/.config\/hmenu\/histFile@
histFile :: IO FilePath
histFile = hmenuPath <<>> "histFile"

-- | Functorial append operation.
infixr 6 <<>>  -- same as <>
(<<>>) :: Functor f => f FilePath -> FilePath -> f FilePath
liftedFp <<>> fp = liftedFp <&> (</> fp)

-- | Combine two paths into a new path.
-- Adapted from: https:\/\/hackage.haskell.org\/package\/filepath
infixr 5 </>
(</>) :: FilePath -> FilePath -> FilePath
(</>) a b
    | hasLeadingPathSeparator b = b
    | otherwise                 = combineAlways a b
  where
    hasLeadingPathSeparator :: FilePath -> Bool
    hasLeadingPathSeparator []    = False
    hasLeadingPathSeparator (x:_) = x == '/'

    -- | Combine two paths, assuming rhs is NOT absolute.
    combineAlways :: FilePath -> FilePath -> FilePath
    combineAlways z w
        | null z        = w
        | null w        = z
        | last z == '/' = z ++ w
        | otherwise     = z ++ "/" ++ w
