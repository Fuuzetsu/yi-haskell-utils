-- |
-- Module      :  Yi.Mode.Haskell.Utils.Internal
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental

module Yi.Mode.Haskell.Utils.Internal where

import           Control.Monad (liftM)
import           Data.Char (isDigit)
import           Data.List
import           Data.List.Split
import           Text.Read (readMaybe)
import           Yi hiding (foldl, (.), notElem, mapM, mapM_)
import qualified Yi.Mode.Interactive as Interactive

-- | Function name and string representing the type.
type HFunction = (String, String)

-- | HConstr denotes the constructor name and number of parameters.
-- We don't particularly care what the parameters are.
type HConstr = (String, Int)

-- | Data type name and list of its constructors. We don't particurarly
-- care for the parameters to the data type for now.
type HDataType = (String, [HConstr])

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

-- | Similar to 'extractFunctions' but extracts a data type instead from the
-- output of ‘:info’. It's /very/ hacky, just a prototype.
--
-- TODO: Record types
extractDataType :: [String] -> HDataType
extractDataType xs =
  let typeStart =
        dropWhile (\x -> not $ "data " `isPrefixOf` x) $ map dropComment xs
      cleanContent = map words (joinSplits typeStart)
      (_:c:rest) = head cleanContent
      ctors = drop 1 $ dropWhile (/= "=") rest

      consts :: [HConstr]
      consts = map (\(y:ys) -> (y, length ys)) $ splitOn ["|"] ctors
  in (c, consts)


-- | Drops trailing comments from lines like
-- @Foo -- defined in Fake.Module@
dropComment :: String -> String
dropComment [] = []
dropComment ('-':'-':_) = []
dropComment (x:xs) = x : dropComment xs


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

test :: [String]
test =
  [ "data Maybe a = Nothing | Just a        -- Defined in `Data.Maybe'"
  , "  | Fake a b c"
  , "  | SuperFake a"
  , "  | LastFake"
  , "instance Eq a => Eq (Maybe a) -- Defined in `Data.Maybe'"
  , "instance Monad Maybe -- Defined in `Data.Maybe'"
  , "instance Functor Maybe -- Defined in `Data.Maybe'"
  , "instance Ord a => Ord (Maybe a) -- Defined in `Data.Maybe'"
  , "instance Read a => Read (Maybe a) -- Defined in `GHC.Read'"
  , "instance Show a => Show (Maybe a) -- Defined in `GHC.Show'"
  , "instance Applicative Maybe -- Defined in `Control.Applicative'"
  , "instance Foldable Maybe -- Defined in `Data.Foldable'"
  , "instance Traversable Maybe -- Defined in `Data.Traversable'"
  , "instance Initializable (Maybe a) -- Defined in `Yi.Prelude'"
  , "instance Binary a => Binary (Maybe a)"
  , "  -- Defined in `binary-0.5.1.1:Data.Binary'"
  , "instance Alternative Maybe -- Defined in `Control.Applicative'"
  ]
