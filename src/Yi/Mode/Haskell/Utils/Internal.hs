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
import           Language.Haskell.GhcMod
import           Yi hiding (foldl, (.), notElem, mapM, mapM_)
import           Yi.IReader (getBufferContents)
import qualified Yi.Mode.Interactive as Interactive

-- | Function name and string representing the type.
type HFunction = (String, String)

-- | HConstr denotes the constructor name and number of parameters.
-- We don't particularly care what the parameters are.
type HConstr = (String, Int)

-- | Data type name and list of its constructors. We don't particurarly
-- care for the parameters to the data type for now.
type HDataType = (String, [HConstr])

data HType = HList | HChar | HDT HDataType
           | HNum | HFunc | HT HDataType deriving (Show, Eq)

getModuleName :: YiM (Either String ModuleString)
getModuleName = do
  c <- filter ("module " `isPrefixOf`) . lines <$> withBuffer getBufferContents
  return $ case c of
    [] -> Left "Couldn't find the module name in the file."
    (x:_) -> Right . takeWhile (/= ' ') . tail $ dropWhile (/= ' ') x

-- | Decide how to possible break up the types.
-- For now we just blindly insert identifiers.
breakType :: HType -> Either String [String]
breakType HList = Right ["[]", "(x:xs)"]
breakType HChar = Left "Can't case-split on a character type."
breakType HNum = Left "Can't case-split on a number type."
breakType HFunc = Left "Can't case-split on functions."
breakType (HDT (_, cs)) =
  Right $ map repArgs cs
  where
    repArgs (cn, 0) = cn
    repArgs (cn, ar) = "(" ++ cn ++ " " ++ mkv ar ++ ")"

    mkv n = unwords $ zipWith (\x y -> x : show y) (replicate n 'x') [1 .. ]

-- | If possible, naively case splits at the variable at point.
--
-- Warning: it will at the moment naively duplicate the line and make the
-- replacement, whether it makes sense for it to do so or not!
caseSplitAtPoint :: YiM ()
caseSplitAtPoint = do
  t <- getTypeAtPoint
  case t >>= breakType of
    Left e -> msgEditor e
    Right [] -> return ()
    Right xs -> do
      cc <- withBuffer curCol
      cl <- withBuffer curLn
      let (y:ys) = reverse xs
          -- We don't duplicate line for last case
          act = reverse $ (y, return ()) : zip ys (repeat duplicateUnder)

      mapM_ (f cc) act
      withBuffer $ moveToLineColB cl cc
      fwriteE -- save after splits
  where
    f c (case', a) = do
      _ <- a
      withBuffer $ do
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
  withBuffer $ do
    cc <- curCol -- remember column position
    curL <- curLn
    cl <- readLnB -- get current line
    moveToSol >> lineDown >> newlineB >> lineUp -- make space
    insertN cl -- insert our copied line
    moveToLineColB curL cc

-- | Uses GhcMod to get the type of the thing at point
getTypeAtPoint :: YiM (Either String HType)
getTypeAtPoint = do
  mn <- getModuleName
  case mn of
    Left err -> return $ Left err
    Right n -> do
      c <- io findCradle
      BufferFileInfo fn _ ln cl _ _ _ <- withBuffer bufInfoB
      msgEditor $ fn ++ ":" ++ n ++ ":" ++ show ln ++ ":" ++ show cl
      ts <- io $ typeExpr defaultOptions c fn n ln cl
      if ":Error:" `isInfixOf` ts
        then return $ Left ts
        else case lines ts of
         [] -> return $ Left "No value at point."
         x:_ -> do
           msgEditor $ "x: " ++ x
           let t = reverse . takeWhile (/= '"') . drop 1 $ reverse x
           msgEditor $ "t: " ++ t
           case head $ words t of
             '[':_ -> return $ Right HList
             x' -> if "->" `isInfixOf` t -- function pre-split
                   then return $ Right HFunc
                   else do
                     msgEditor $ fn ++ " -- " ++ n ++ " -- " ++ x'
                     inf <- io $ getTypeInfo defaultOptions c fn n x'
                     return $ if ":Error:" `isInfixOf` inf
                       then Left inf
                       else Right . processType . extractDataType $ lines inf

-- | Wrapper around 'infoExpr' which will follow ‘type’ declarations before
-- settling for something.
getTypeInfo :: Options -> Cradle -> FilePath -> ModuleString
            -> Expression -> IO String
getTypeInfo o c fp m e = do
  inf <- infoExpr o c fp m e
  let tname = fst . extractDataType $ lines inf
  if "type " `isPrefixOf` inf
    then infoExpr o c fp m tname
    else return inf

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
    comb xs' (' ':y) = init xs' ++ [last xs' ++ y]
    comb xs' y = xs' ++ [y]

test :: [String]
test =
  [ "type Maybe a = Nothing | Just a        -- Defined in `Data.Maybe'"
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


testM = [ "-- You should have received a copy of the GNU General Public License"
        , "-- along with this program.  If not, see <http://www.gnu.org/licenses/>."
        , ""
        , "-- |"
        , "-- Module      :  Main"
        , "-- Description :  Coursework 2 submission for CM20219 course ran in"
        , "--                University of Bath in 2013. Submission by mk440"
        , "-- Copyright   :  (c) Mateusz Kowalczyk 2013"
        , "-- License     :  GPLv3"
        , "-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk"
        , ""
        , "{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}"
        , "{-# OPTIONS_GHC -fno-warn-orphans #-}"
        , "module Main where"
        , ""
        , ""
        , "import Control.Applicative"
        , "import Control.Monad"
        , "import Control.Lens"
        , "import Data.Time.Clock"
        , "import Graphics.Rendering.OpenGL"
        , "import Graphics.UI.GLUT"
        , "import Data.IORef"
        , "import System.Exit"
        , ""
        , "-- | Specifies a glVertex3f from a 'GLfloat' triple."
        , "vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()"
        , "vertex3f (x, y, z) = vertex $ Vertex3 x y z"
        ]
