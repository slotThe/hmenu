module Core.Parser
    ( -- * Parse the given file
      getHist  -- :: FilePath -> IO Items

      -- * The actual parser
    , pFile    -- :: ByteString -> Items
    ) where

import Core.Util (Items)

import qualified Data.ByteString.Char8 as BS
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
    go its input = go ((name, number) : its) rest'
      where
        (name  , rest ) :: (ByteString, ByteString)
            = BS.drop 1 <$> BS.span (/= ' ') input
        (number, rest') :: (Double, ByteString)
            = maybe (0, "") (BS.drop 1 <$>) $ readDecimal rest
