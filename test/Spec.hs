{-# LANGUAGE TypeApplications  #-}

module Main
    ( main
    ) where

-- Local imports
import Core.Parser (pMap)
import Core.Select (showItems)
import Core.Util (Items, (</>))

-- ByteString
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Map
import qualified Data.Map as Map

-- Testing
import Test.Hspec (describe, hspec, it)
import Test.Hspec.Attoparsec (shouldSucceedOn)
import Test.QuickCheck
    ( Gen, arbitrary, forAll, getASCIIString, listOf, property, suchThat )

-- Other imports
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.Either (fromRight)


main :: IO ()
main = hspec $ do

    describe "pMap" $
        it "should succeed on ascii strings and positive integers" $
            forAll itemsGen $ \x -> pMap `shouldSucceedOn` showItems x

    describe "pMap" $
        it "should satisfy x == (parse . pretty) x" $
            forAll itemsGen $ \x ->
                (fromRight Map.empty . parseOnly pMap . showItems) x == x

    -- Source: https://hackage.haskell.org/package/filepath
    describe "</>" $
        it "\"/directory\" </> \"file.ext\" == \"/directory/file.ext\"" $ property $
            "/directory" </> "file.ext" == "/directory/file.ext"
    describe "</>" $
        it "\"directory\" </> \"/file.ext\" == \"/file.ext\"" $ property $
            "directory" </> "/file.ext" == "/file.ext"
    describe "</>" $
        it "\"/\" </> \"test\" == \"/test\"" $ property $
            "/" </> "test" == "/test"
    describe "</>" $
        it "\"home\" </> \"bob\" == \"home/bob\"" $ property $
            "home" </> "bob" == "home/bob"
    describe "</>" $
        it "\"x:\" </> \"foo\" == \"x:/foo\"" $ property $
            "x:" </> "foo" == "x:/foo"
    describe "</>" $
        it "\"home\" </> \"/bob\" == \"/bob\"" $ property $
            "home" </> "/bob" == "/bob"

itemsGen :: Gen Items
itemsGen = Map.fromList <$> listOf itemGen

itemGen :: Gen (ByteString, Int)
itemGen = do
    k <- bytestringGen
    v <- arbitrary @Int `suchThat` (>= 0)
    pure (k, v)

bytestringGen :: Gen ByteString
bytestringGen =
    BS.pack <$> (getASCIIString <$> arbitrary) `suchThat` notElem ' '
