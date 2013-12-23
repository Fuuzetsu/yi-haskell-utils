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

import           Data.List
import           Data.Maybe (catMaybes)
import           Yi hiding (foldl, (.), notElem, mapM, mapM_)
import           Yi.Buffer.Misc (elemsB)
import qualified Yi.Mode.Interactive as Interactive
import           Yi.Mode.Haskell.Utils.Internal

-- | Load current buffer in GHCi, checking whether it succeeded.
ghciLoadBufferBool :: YiM Bool
ghciLoadBufferBool = do
    fwriteE
    f <- withBuffer (gets file)
    case f of
      Nothing -> return False
      Just filename -> do
        buf <- ghciGet
        r <- Interactive.queryReply buf (":load " ++ filename)
        return $ "Ok, modules loaded:" `isInfixOf` r

-- | Inserts missing type signatures for functions, above their definitions.
ghciInsertMissingTypes :: YiM ()
ghciInsertMissingTypes = do
  -- Reload GHCi buffer before we ask for locations
  loadS <- ghciLoadBufferBool
  case loadS of
    False -> return ()
    True -> do
      buf <- ghciGet
      result <- Interactive.queryReply buf ":browse"
      bufContent <- withBuffer elemsB
      let funcs = extractFunctions $ lines result
          bufferFuncs = extractFunctions $ lines bufContent
          bufFuncNames = map fst bufferFuncs
          missingFuncSigs = filter (\x -> fst x `notElem` bufFuncNames) funcs
      insertLocs <- catMaybes <$> mapM (`getFuncDefLoc` buf) missingFuncSigs
      let sortedLocs = sortBy (on compare snd) insertLocs
          -- As we're going to be inserting, we need to increase subsequent
          -- insert locations by one
          incSecond _ [] = []
          incSecond n ((s, i):xs) = (s, i + n) : incSecond (n + 1) xs

          increasedLocs :: [(String, Int)]
          increasedLocs = incSecond 0 sortedLocs
          putSig (s, i) = moveToLineColB i 0 >> newlineB
                          >> gotoLnFrom (-1) >> insertN s
      mapM_ (withBuffer . putSig) increasedLocs
