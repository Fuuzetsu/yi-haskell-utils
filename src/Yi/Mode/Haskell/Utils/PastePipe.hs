{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Yi.Mode.Haskell.Utils.PastePipe
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Utility functions to deal with "Utils.PastePipe" which lets us
-- paste the buffer to <lpaste.net>. All functions here work on
-- a region if selected, otherwise on the full buffer.

module Yi.Mode.Haskell.Utils.PastePipe
    ( lpasteBufferDefaults
    , lpasteCustom
    , lpasteWithPrompt
    ) where

import           Control.Applicative
import           Control.Exception
import           Control.Lens (use)
import           Data.Binary
import           Data.Default
import           Data.DeriveTH
import qualified Data.Text as T
import           Data.Typeable
import           System.Environment (getEnv)
import           Utils.PastePipe
import           Yi
import           Yi.MiniBuffer
import           Yi.Monad (gets)
import qualified Yi.Rope as R
import           Yi.String (showT)
import           Yi.Types (YiVariable)
import           Yi.Utils (io)

-- | Config used to use as 'YiVariable'.
data PastePipeConfig = PastePipeConfig (Maybe String) String
                       -- ^ Maybe a username to use and a language
                       -- string to use. The 'Default' instance uses
                       -- Nothing and \"haskell\".
                     deriving (Show, Eq, Typeable)

instance Default PastePipeConfig where
  def = PastePipeConfig Nothing "haskell"

$(derive makeBinary ''PastePipeConfig)

instance YiVariable PastePipeConfig

-- | Posts with default values determined by 'PastePipeConfig' and
-- the default values set by PastePipe's 'config' function.
lpasteBufferDefaults :: YiM ()
lpasteBufferDefaults = do
  fn <- withCurrentBuffer (gets file)
  withCurrentBuffer getBufferDyn >>= \case
    PastePipeConfig Nothing l -> do
      user <- io $ getEnv "USER"
      let c = (config user) { language = l
                            , title = maybe "PipePaste pasted text" id fn }
      withCurrentBuffer elemsB >>= go c >>= printMsg . T.pack
    PastePipeConfig (Just n) l ->
      let c = (config n) { language = l
                         , title = maybe "PipePaste pasted text" id fn }
      in getContent >>= go c >>= printMsg . T.pack
  where
    go c = io . catchP . fmap show . post c . R.toString

    catchP :: IO String -> IO String
    catchP a = a `catch` \(e :: SomeException) ->
      return $ "Failed to paste: " ++ show e

-- | As 'lpasteCustom' but interatively prompts for all the values.
lpasteWithPrompt :: YiM ()
lpasteWithPrompt = do
  userEnv <- io $ getEnv "USER"
  PastePipeConfig n _ <- withCurrentBuffer getBufferDyn
  let users = T.pack <$> case n of
        Nothing -> [userEnv]
        Just x -> [userEnv, x]

  withMinibufferFin "User:" users $ \u ->
    withMinibufferFree "Title: " $ \t ->
      withMinibufferFree "Language: " $ \l ->
        lpasteCustom (T.unpack u) (Just $ T.unpack t) (T.unpack l)

-- | Takes a username, title and a language and pastes to <lpaste.net> with
-- values.
lpasteCustom :: String -> Maybe String -> String -> YiM ()
lpasteCustom u t l = do
  c <- withCurrentBuffer getSelectRegionB >>= \r ->
    withCurrentBuffer $ if regionStart r == regionEnd r
      then elemsB
      else readRegionB r
  let conf = (config u) { title = maybe "PipePaste pasted text" id t
                        , language = l }
  io (post conf $ R.toString c) >>= printMsg . showT

-- | If we have a region selected, we return that. If not, we return the full
-- file.
getContent :: YiM R.YiString
getContent = withCurrentBuffer getSelectRegionB >>= \r ->
  withCurrentBuffer $ if regionStart r == regionEnd r
                      then elemsB
                      else readRegionB r
