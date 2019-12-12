module Core.Parser
    ( getHist
    ) where

-- ByteString
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BS

-- Qualified imports
import qualified Text.Megaparsec.Byte.Lexer as L

-- Other imports
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec
    ( ParseErrorBundle
    , Parsec
    , between
    , choice
    , runParser
    , sepBy
    , some
    )
import Text.Megaparsec.Byte (alphaNumChar, char, space, string)


-- | Simple parser type.
type Parser a = Parsec Void ByteString a

pMap :: Parser [(ByteString, Int)]
pMap = do
  _ <- string "fromList "
  between (char 91) (char 93) (pKeyValue `sepBy` char 44)

pKeyValue :: Parser (ByteString, Int)
pKeyValue =
    between (char 40) (char 41) $ do
        b <- between (char 34) (char 34) (bsome posixChar)
        _ <- char 44
        v <- integer
        return (b, v)
  where
    posixChar = choice
        [ alphaNumChar
        , char 43
        , char 45
        , char 46
        , char 47
        , char 58
        , char 91
        , char 93
        , char 95
        ]

-- | Convert a Character parser to a Text parser.
bsome :: Parser Word8 -> Parser ByteString
bsome = fmap B.pack . some

integer :: Parser Int
integer = L.lexeme space L.decimal

getHist
    :: FilePath
    -> IO (Either (ParseErrorBundle ByteString Void) [(ByteString, Int)])
getHist file = runParser pMap "" <$> BS.readFile file
