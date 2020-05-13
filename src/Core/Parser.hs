module Core.Parser
    ( -- * Parse the given file
      getHist  -- :: FilePath -> IO Items

      -- * The actual parser
    , pFile    -- :: ByteString -> [(ByteString, Int)]
    ) where

import Core.Util (Items)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as Map

import Data.ByteString (ByteString)


-- | Read a history file (the file must exist) and parse it into an item map.
getHist :: FilePath -> IO Items
getHist file = Map.fromList . pFile <$> BS.readFile file

-- | Parse a history file of name-number pairs.
pFile :: ByteString -> [(ByteString, Int)]
pFile h = go [] h
  where
    go its ""    = its
    go its input = go ((name, number) : its) rest'
      where
        (name  , rest ) = BS.drop 1 <$> BS.span (/= ' ') input
        (number, rest') = maybe (0, "") (BS.drop 1 <$>) $ BS.readInt rest
