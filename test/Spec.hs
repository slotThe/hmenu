{-# LANGUAGE TypeApplications  #-}

module Main
    ( main
    ) where

import Core.Parser (pFile)
import Core.Select (showItems)
import Core.Util (Items, (</>))

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as Map

import Data.ByteString (ByteString)
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck
    ( Gen, arbitrary, forAll, getASCIIString, listOf, property, suchThat )


main :: IO ()
main = hspec $ do
  describe "pFile" $
    it "should satisfy x == (parse . pretty) x" $
      forAll itemsGen $ \x ->
        (Map.fromList . pFile . showItems) x == x

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
itemGen = (,) <$> bytestringGen <*> arbitrary @Int `suchThat` (>= 0)

bytestringGen :: Gen ByteString
bytestringGen =
    BS.pack <$> (getASCIIString <$> arbitrary) `suchThat` notElem ' '
