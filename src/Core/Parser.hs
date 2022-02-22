module Core.Parser
    ( -- * Parse the given file
      getHist  -- :: FilePath -> IO Items

      -- * The actual parser
    , pFile    -- :: ByteString -> Items
    ) where

import Core.Util (Items)

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lex.Fractional (readDecimal)


-- | Read a history file (the file must exist) and parse it into a map.
getHist :: FilePath -> IO Items
getHist = fmap pFile . BS.readFile

-- | Parse a history file of name-number pairs.
pFile :: ByteString -> Items
pFile = fromList . go []
  where
    go :: [(ByteString, Double)] -> ByteString -> [(ByteString, Double)]
    go its ""    = its
    go its input = go (it : its) rest
      where
        (it, rest) :: ((ByteString, Double), ByteString)
            = bimap (second (maybe 0 fst . readDecimal) . spanEnd (/= ' '))
                    (BS.drop 1)
            . BS.span (/= '\n')
            $ input

    spanEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
    spanEnd p bs = (init' $ BS.dropWhileEnd p bs, BS.takeWhileEnd p bs)

    init' :: ByteString -> ByteString
    init' = maybe "" fst . BS.unsnoc
{-# INLINE pFile #-}
