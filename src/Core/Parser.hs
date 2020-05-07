module Core.Parser
    ( getHist
    , pMap
    ) where

import Core.Util (Items)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as Map

import Control.Applicative (many)
import Data.Attoparsec.ByteString.Char8
    ( Parser, char8, decimal, endOfLine, parseOnly, takeTill )
import Data.ByteString (ByteString)


-- | Parse a given file.
-- This will be the history file, hence the suggestive name.
getHist :: FilePath -> IO (Either String Items)
getHist file = parseOnly pMap <$> BS.readFile file

-- | Parse a 'Map ByteString Int' (aka. 'Items') that's written line by line.
pMap :: Parser Items
pMap = Map.fromList <$> many (pKeyValue <* endOfLine)

-- | Parse a single key-value pair of the 'Items' type.
pKeyValue :: Parser (ByteString, Int)
pKeyValue = do
    k <- takeTill (== ' ')
    _ <- char8 ' '
    v <- decimal
    pure (k, v)
