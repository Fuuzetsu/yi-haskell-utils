{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Yi.Mode.Haskell.Utils
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental

module Yi.Mode.Haskell.Utils
       ( ghciLoadBufferBool
       , ghciInsertMissingTypes
       , getTypeAtPoint
       , caseSplitAtPoint
       )
       where

import           Control.Applicative ((<$>))
import           Data.Function (on)
import           Data.List
import           Data.Maybe (catMaybes)
import           Data.Monoid
import qualified Data.Text as T
import           Yi
import           Yi.Mode.Haskell.Utils.Internal
import qualified Yi.Mode.Interactive as Interactive
import           Yi.Monad (gets)
import qualified Yi.Rope as R

-- | Load current buffer in GHCi, checking whether it succeeded.
ghciLoadBufferBool :: YiM Bool
ghciLoadBufferBool = do
  fwriteE
  f <- withCurrentBuffer (gets file)
  case f of
    Nothing -> return False
    Just filename -> do
      buf <- ghciGet
      r <- Interactive.queryReply buf (":load " <> filename)
      return $ "Ok, modules loaded:" `T.isInfixOf` R.toText r

-- | Inserts missing type signatures for functions, above their definitions.
ghciInsertMissingTypes :: YiM ()
ghciInsertMissingTypes = do
  -- Reload GHCi buffer before we ask for locations
  ghciLoadBufferBool >>= \case
    False -> return ()
    True -> do
      buf <- ghciGet
      result <- Interactive.queryReply buf ":browse"
      bufContent <- withCurrentBuffer elemsB
      let funcs = extractFunctions . map R.toString $ R.lines result
          bufferFuncs = extractFunctions . map R.toString $ R.lines bufContent
          bufFuncNames = map fst bufferFuncs
          missingFuncSigs = filter (\x -> fst x `notElem` bufFuncNames) funcs
      insertLocs <- catMaybes <$> mapM (`getFuncDefLoc` buf) missingFuncSigs
      let sortedLocs = sortBy (on compare snd) insertLocs
          -- As we're going to be inserting, we need to increase subsequent
          -- insert locations by one
          fromS = R.fromString
          incSecond _ [] = []
          incSecond n ((s, i):xs) = (fromS s, i + n) : incSecond (n + 1) xs

          increasedLocs :: [(R.YiString, Int)]
          increasedLocs = incSecond 0 sortedLocs
          putSig (s, i) = moveToLineColB i 0 >> newlineB
                          >> gotoLnFrom (-1) >> insertN s
      mapM_ (withCurrentBuffer . putSig) increasedLocs
