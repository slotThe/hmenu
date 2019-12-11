module Core.Util
    ( tryAddPrefix
    , clean
    ) where

-- Other imports
import System.FilePath ((</>))


{- | Add a prefix to a string if the string is not starting with "/".
   This will ensure the user can specify absolute paths to files, but also
   conveniently use relative paths (starting from '$HOME') if that is desired.
-}
tryAddPrefix :: FilePath -> FilePath -> FilePath
tryAddPrefix prefix xs
    | null xs   = ""
    | x == '/'  = xs
    | otherwise = prefix </> xs
  where
    x = head xs

-- | Clean a string up until (and including) the first colon.
clean :: String -> String
clean = (" " ++) . drop 1 . dropWhile (/= ':')
