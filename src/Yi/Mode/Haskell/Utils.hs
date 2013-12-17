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
       )
       where

import           Control.Monad (liftM)
import           Data.Char (isDigit)
import           Data.List
import           Data.Maybe (catMaybes)
import           Text.Read (readMaybe)
import           Yi hiding (foldl, (.), notElem, mapM, mapM_)
import           Yi.IReader (getBufferContents)
import qualified Yi.Mode.Interactive as Interactive

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


type HFunction = (String, String)

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
      bufContent <- withBuffer getBufferContents
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

-- | Asks GHCi about the location of a function definition in the file.
-- We use this as a helper for 'ghciInsertMissingTypes'
getFuncDefLoc :: HFunction -> BufferRef -> YiM (Maybe (String, Int))
getFuncDefLoc (funcName, t) g = do
  infoReply <- Interactive.queryReply g (":info " ++ funcName)
  let f :: String -> Maybe Int
      f = readMaybe . reverse . takeWhile isDigit
          . tail . dropWhile (/= ':') . reverse

  return $ case f infoReply of
    Nothing -> Nothing
    Just r -> Just (funcName ++ " :: " ++ t, r)


-- | Takes an \n-separated output of @:browse@ and returns a list of 'HFunction'
-- describing the function name and its type. Anything that's not a function
-- is ignored.
extractFunctions :: [String] -> [HFunction]
extractFunctions = filtFuncs fs . joinSplits
  where
    filtFuncs f xs = [ (x, unwords ys) | Just ([x], _:ys) <- map f xs ]
    fs x = let w = words x
           in flip splitAt w `liftM` elemIndex "::" w

-- | Joins up lines split up by GHCi to fit nicely in the output window. e.g.
--
-- @
-- foo ::
--   Int
--   -> Int
-- @
--
-- becomes @foo :: Int -> Int@
joinSplits :: [String] -> [String]
joinSplits [] = []
joinSplits (x:xs) = Prelude.foldl comb [x] xs
  where
    comb :: [String] -> String -> [String]
    comb xs' (' ':y) = init xs' ++ [last xs' ++ y]
    comb xs' y = xs' ++ [y]
