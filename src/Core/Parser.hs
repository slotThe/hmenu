module Core.Parser
    ( getHist
    , pMap
    ) where

-- Local imports
import Core.Util (Items)

-- ByteString
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Map
import qualified Data.Map.Strict as Map

-- Other imports
import Control.Applicative (many)
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char8
    , decimal
    , endOfLine
    , parseOnly
    , takeTill
    )


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
