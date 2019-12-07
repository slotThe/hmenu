module Core.Util
    ( splitOnColon
    , tryAddPrefix
    , clean
    ) where

{- | Split a string into a list of strings, with the split point being a colon.

   Adapted from:
     https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:words
-}
splitOnColon :: String -> [String]
splitOnColon s =
    case dropWhile isColon s of
        "" -> []
        s' -> w : splitOnColon s''
            where (w, s'') = break isColon s'
  where
    isColon :: Char -> Bool
    isColon = (== ':')

{- | Add a prefix to a string if the string is not starting with "/".
   This will ensure the user can specify absolute paths to files, but also
   conveniently use relative paths (starting from '$HOME') if that is desired.
-}
tryAddPrefix :: String -> String -> String
tryAddPrefix prefix xs
    | null xs   = ""
    | x == '/'  = xs
    | otherwise = prefix <> xs
  where
    x = head xs

-- | Clean a string up until (and including) the first colon.
clean :: String -> String
clean = (" " ++) . drop 1 . dropWhile (/= ':')
