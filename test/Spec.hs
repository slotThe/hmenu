{-# LANGUAGE TypeApplications #-}

module Main
    ( main  -- :: IO ()
    ) where

import Core.Parser (pFile)
import Core.Select (showItems)
import Core.Util (Items, (</>))

import qualified Data.ByteString.Char8 as BS

import Test.Hspec (describe, hspec, it)
import Test.QuickCheck (Gen, arbitrary, forAll, getASCIIString, listOf, property, suchThat)

main :: IO ()
main = hspec $ do
  describe "pFile" $
    it "should satisfy id ≈ parse ∘ pretty (up to rounding sadness)" $
      forAll itemsGen \x -> toList x =~ toList (pFile (showItems x))

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
itemsGen = fromList <$> listOf itemGen

itemGen :: Gen (ByteString, Double)
itemGen = (,) <$> bytestringGen <*> arbitrary @Double `suchThat` (>= 0)

bytestringGen :: Gen ByteString
bytestringGen = BS.pack <$> (getASCIIString <$> arbitrary)
                              `suchThat` ((&&) <$> notElem ' ' <*> notElem '\n')

(=~) :: [(ByteString, Double)] -> [(ByteString, Double)] -> Bool
((n, x) : xs) =~ ((m, y) : ys)
  | n == m && x `approx` y = xs =~ ys
  | otherwise              = False
 where
  approx :: Double -> Double -> Bool
  approx a b = abs (a - b) < 1e-14
[] =~ [] = True
_  =~ _  = False
