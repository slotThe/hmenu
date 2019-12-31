module Core.Parser
    ( getHist
    ) where

-- ByteString
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Other imports
import Control.Applicative (some)
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
getHist :: FilePath -> IO (Either String [(ByteString, Int)])
getHist file = parseOnly pMap <$> BS.readFile file

-- | Parse a 'Map ByteString Int' (aka. 'Items') that's written line by line.
pMap :: Parser [(ByteString, Int)]
pMap = some $ pKeyValue <* endOfLine

-- | Parse a single key-value pair of the 'Items' type.
pKeyValue :: Parser (ByteString, Int)
pKeyValue = do
    k <- takeTill (== ' ')
    _ <- char8 ' '
    v <- decimal
    pure (k, v)
