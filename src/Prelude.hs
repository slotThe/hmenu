{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Prelude
    ( module BasePrelude
    , ByteString
    , Map
    , XdgDirectory(XdgConfig)
    , ifM
    , doesFileExist
    , proc
    , getXdgDirectory
    , spawnCommand
    , createDirectoryIfMissing
    ) where

import BasePrelude
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import System.Directory (XdgDirectory(XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Process (proc, spawnCommand)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y
{-# INLINE ifM #-}
