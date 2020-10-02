module Core.Parser
    ( -- * Parse the given file
      getHist  -- :: FilePath -> IO Items

      -- * The actual parser
    , pFile    -- :: ByteString -> Items
    ) where

import Core.Util (Items)

import qualified Data.ByteString.Char8 as BS


-- | Read a history file (the file must exist) and parse it into an item map.
getHist :: FilePath -> IO Items
getHist = fmap pFile . BS.readFile

-- | Parse a history file of name-number pairs.
pFile :: ByteString -> Items
pFile = fromList . go []
  where
    go :: [(ByteString, Int)] -> ByteString -> [(ByteString, Int)]
    go its ""    = its
    go its input = go ((name, number) : its) rest'
      where
        (name  , rest ) :: (ByteString, ByteString)
            = BS.drop 1 <$> BS.span (/= ' ') input
        (number, rest') :: (Int, ByteString)
            = maybe (0, "") (BS.drop 1 <$>) $ BS.readInt rest
