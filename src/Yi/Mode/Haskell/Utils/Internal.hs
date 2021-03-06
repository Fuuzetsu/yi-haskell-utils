{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Yi.Mode.Haskell.Utils.Internal
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental

module Yi.Mode.Haskell.Utils.Internal where

import           Control.Applicative ((<$>))
import           Control.Monad (liftM, void)
import           Data.Char (isDigit)
import           Data.List
import           Data.List.Split
import           Data.Monoid
import qualified Data.Text as T
import           Language.Haskell.GhcMod
import           Text.Read (readMaybe)
import           Yi
import qualified Yi.Mode.Interactive as Interactive
import qualified Yi.Rope as R
import           Yi.String (showT)
import           Yi.Utils (io)

-- | Function name and string representing the type.
type HFunction = (String, String)

-- | HConstr denotes the constructor name and number of parameters.
-- We don't particularly care what the parameters are.
type HConstr = (String, Int)

-- | Data type name and list of its constructors. We don't particurarly
-- care for the parameters to the data type for now.
type HDataType = (String, [HConstr])

data HType = HList | HChar | HDT HDataType
           | HNum | HFunc deriving (Show, Eq)

getModuleName :: YiM (Either T.Text ModuleString)
getModuleName = do
  bufLines <- T.lines . R.toText <$> withCurrentBuffer elemsB
  return $ case mods bufLines of
    [] -> Left "Couldn't find the module name in the file."
    (x:_) -> Right . T.unpack . T.takeWhile (/= ' ') $ dropMod x
  where
    dropMod = T.drop (T.length "module ")
    mods x = filter (T.isPrefixOf "module ") x

-- | Decide how to possible break up the types.
-- For now we just blindly insert identifiers.
breakType :: HType -> Either T.Text [String]
breakType HList = Right ["[]", "(x:xs)"]
breakType HChar = Left "Can't case-split on a character type."
breakType HNum = Left "Can't case-split on a number type."
breakType HFunc = Left "Can't case-split on functions."
breakType (HDT (_, cs)) =
  Right $ map repArgs cs
  where
    repArgs (cn, 0) = cn
    repArgs (cn, ar) = "(" <> cn <> " " <> mkv ar <> ")"
    i = [1 .. ] :: [Int]
    mkv n = unwords $ zipWith (\x y -> x : show y) (replicate n 'x') i

-- | If possible, naively case splits at the variable at point.
--
-- Warning: it will at the moment naively duplicate the line and make the
-- replacement, whether it makes sense for it to do so or not!
caseSplitAtPoint :: YiM ()
caseSplitAtPoint = do
  t <- getTypeAtPoint
  case t >>= breakType of
    Left e -> printMsg e
    Right [] -> return ()
    Right xs -> do
      cc <- withCurrentBuffer curCol
      cl <- withCurrentBuffer curLn
      let (y:ys) = R.fromString <$> reverse xs
          -- We don't duplicate line for last case
          act = reverse $ (y, return ()) : zip ys (repeat duplicateUnder)

      mapM_ (f cc) act
      withCurrentBuffer $ moveToLineColB cl cc
      void fwriteE -- save after splits
  where
    f c (case', a) = do
      _ <- a
      withCurrentBuffer $ do
        bkillWordB
        insertN case'
        lineDown
        moveToColB c

{- |
Duplicates the current line down. Denoting the cursor
with the ‘|’ character:

@
foo
ba|r
baz
@

would become

@
foo
ba|r
bar
baz
@
-}
duplicateUnder :: YiM ()
duplicateUnder = do
  withCurrentBuffer $ do
    cc <- curCol -- remember column position
    curL <- curLn
    cl <- readLnB -- get current line
    moveToSol >> lineDown >> newlineB >> lineUp -- make space
    insertN cl -- insert our copied line
    moveToLineColB curL cc

-- | Uses GhcMod to get the type of the thing at point
getTypeAtPoint :: YiM (Either T.Text HType)
getTypeAtPoint = getModuleName >>= \case
  Left err -> return $ Left err
  Right n -> do
    BufferFileInfo fn _ ln cl _ _ _ <- withCurrentBuffer bufInfoB
    printMsg $ T.pack fn <> ":" <> T.pack n <> ":" <> showT ln <> ":" <> showT cl
    (ts, _) <- io $ runGhcModT defaultOptions (types fn ln cl)
    case ts of
      Left s -> return . Left $
                "ghc-mod: " <> T.pack "ghc-mod failed with no error message"
      Right s -> case lines s of
        [] -> return $ Left "No value at point."
        x:_ -> do
          printMsg $ "x: " <> T.pack x
          let t = reverse . takeWhile (/= '"') . drop 1 $ reverse x
          printMsg $ "t: " <> T.pack t
          case head $ words t of
            '[':_ -> return $ Right HList
            x' -> if "->" `isInfixOf` t -- function pre-split
                  then return $ Right HFunc
                  else do
                    printMsg $ T.pack fn <> " -- " <> T.pack n
                                <> " -- " <> T.pack x'
                    inf <- io $ getTypeInfo defaultOptions fn n x'
                    return $ if ":Error:" `T.isInfixOf` T.pack inf
                      then Left $ T.pack inf
                      else Right . processType . extractDataType $ lines inf

-- | Wrapper around 'infoExpr' which will follow ‘type’ declarations before
-- settling for something.
getTypeInfo :: Options -> FilePath -> ModuleString
            -> Expression -> IO String
getTypeInfo o fp m e = do
  (inf, _) <- runGhcModT o (info fp e)
  case inf of
    Left e' -> return $ show e'
    Right s -> do
      let tname = fst . extractDataType $ lines s
      if "type " `isPrefixOf` s
        then getTypeInfo o fp m tname
        else return s

-- | Here we further process the type returned to us. We can't case split on
-- on everything and GhcMod won't take anything paramerised to begin with
-- so we try to catch some basic types here.
processType :: HDataType -> HType
processType ('[':_, _) = HList
processType ('I':'n':'t':_, _) = HNum -- false positives ahoy
processType ("Float", _) = HNum
processType ("Double", _) = HNum
processType ("Point", _) = HNum
processType ("Size", _) = HNum
processType ("Char", _) = HChar
processType dt
  | "->" `isInfixOf` fst dt = HFunc
  | otherwise = HDT dt


-- | Asks GHCi about the location of a function definition in the file.
-- We use this as a helper for 'ghciInsertMissingTypes'
getFuncDefLoc :: HFunction -> BufferRef -> YiM (Maybe (String, Int))
getFuncDefLoc (funcName, t) g = do
  infoReply <- Interactive.queryReply g (":info " <> funcName)
  let f :: R.YiString -> Maybe Int
      f = readMaybe . T.unpack . T.reverse . T.takeWhile isDigit
          . T.tail . T.dropWhile (/= ':') . T.reverse . R.toText

  return $ case f infoReply of
    Nothing -> Nothing
    Just r -> Just (funcName <> " :: " <> t, r)


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
  let notDoT x = not $ "data " `isPrefixOf` x
                       || "type " `isPrefixOf` x
                       || "newtype " `isPrefixOf` x
      typeStart =
        dropWhile notDoT $ map dropComment xs
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
    comb xs' (' ':y) = init xs' <> [last xs' <> y]
    comb xs' y = xs' <> [y]
