module Core.Parser
    ( getHist
    ) where

-- ByteString
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Other imports
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char8
    , decimal
    , eitherResult
    , parse
    , sepBy
    , string
    , takeTill
    )


-- | Parse a given file.
-- This will be the history file, hence the suggestive name.
getHist :: FilePath -> IO (Either String [(ByteString, Int)])
getHist file = eitherResult . parse pMap <$> BS.readFile file

-- | Parse a 'Map ByteString Int' (aka. 'Items') that has been converted to a
-- list via 'Map.toList'.
pMap :: Parser [(ByteString, Int)]
pMap = do
    _ <- string "fromList "
    between (char8 '[') (char8 ']') $ pKeyValue `sepBy` char8 ','

-- | Parse a single key-value pair of the 'Items' type.
pKeyValue :: Parser (ByteString, Int)
pKeyValue =
    between (char8 '(') (char8 ')') $ do
        k <- between (char8 '\"') (char8 '\"') $ takeTill (== '\"')
        _ <- char8 ','
        v <- decimal
        return (k, v)

-- | Parse something of the form 'open EXPRESSION close'
between :: Applicative m => m open -> m close -> m a -> m a
between open close p = open *> p <* close
