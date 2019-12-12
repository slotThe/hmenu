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


getHist :: FilePath -> IO (Either String [(ByteString, Int)])
getHist file = eitherResult . parse pMap <$> BS.readFile file

pMap :: Parser [(ByteString, Int)]
pMap = do
    _ <- string "fromList "
    between (char8 '[') (char8 ']') (pKeyValue `sepBy` char8 ',')

pKeyValue :: Parser (ByteString, Int)
pKeyValue =
    between (char8 '(') (char8 ')') $ do
        b <- between (char8 '\"') (char8 '\"') $ takeTill (== '\"')
        _ <- char8 ','
        v <- integer
        return (b, v)

integer :: Parser Int
integer = decimal

between :: Applicative m => m open -> m close -> m a -> m a
between open close p = open *> p <* close
