
module Main (main) where


import Graphics.X11.Turtle

import qualified Data.Map as M

import System.Environment

import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers
import Text.Printf ()
import Text.XML.YJSVG


data LSLarArg = LSLarArg Double deriving (Eq, Show)


data LSAngArg = LSAngArg Double deriving (Eq, Show)


data LSAxiArg = LSAxiArg Char deriving (Eq, Show)


data LSLSysSeq = LSRotateLeft
                 | LSRotateRight
                 | LSMoveForwardT
                 | LSMoveForward
                 | LSMoveBackT
                 | LSMoveBack
                 | LSPushStack
                 | LSPopStack
                 | LSSubSeq Char
                   deriving (Eq)


data LSLSysComp = LSLSysComp {
  cSaxi          :: Char
  , cSseq        :: [LSLSysSeq]
  } deriving (Eq, Show)


data LSysCompSet = LSysCompSet {
  lLar           :: LSLarArg
  , lAng         :: LSAngArg
  , lAxi         :: LSAxiArg
  , lSeqI        :: [LSLSysComp]
  , lIter        :: Integer
  } deriving (Eq, Show)


instance Show LSLSysSeq where
  show a = case a of
                LSRotateLeft     -> " + "
                LSRotateRight    -> " - "
                LSMoveForwardT   -> " F "
                LSMoveForward    -> " f "
                LSMoveBackT      -> " B "
                LSMoveBack       -> " b "
                LSPushStack      -> " [ "
                LSPopStack       -> " ] "
                LSSubSeq n       -> " :" ++ [n] ++ " "


readAxiArg :: LSAxiArg -> Char
readAxiArg (LSAxiArg n) = n


readAngArg :: LSAngArg -> Double
readAngArg (LSAngArg n) = n


readLarArg :: LSLarArg -> Double
readLarArg (LSLarArg n) = n


parEol :: Parser String
parEol = many $ oneOf "\r\n"


parLsLar :: Parser LSLarArg
parLsLar = do
  _ <- string "lar"
  _ <- space
  x <- parseFloat
  _ <- parEol
  return $ LSLarArg x


parLsAng :: Parser LSAngArg
parLsAng = do
  _ <- string "ang"
  _ <- space
  x <- parseFloat
  _ <- parEol
  return $ LSAngArg x


parLsAxi :: Parser LSAxiArg
parLsAxi = do
  _ <- string "axi"
  _ <- space
  x <- oneOf $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']
  _ <- parEol
  return $ LSAxiArg (x :: Char)


parLsSysSeq :: Parser LSLSysSeq
parLsSysSeq = do
  i <- oneOf $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "+-[]"
  case i of
       '-' -> return LSRotateRight
       '+' -> return LSRotateLeft
       'f' -> return LSMoveForward
       'b' -> return LSMoveBack
       '[' -> return LSPushStack
       ']' -> return LSPopStack
       _   -> return $ LSSubSeq i


parLSysComp :: Parser LSLSysComp
parLSysComp = do
  i <- oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  _ <- char ':'
  s <- many parLsSysSeq
  _ <- parEol
  return LSLSysComp {
    cSaxi = i
    , cSseq = s
    }


parLsIterComp :: Parser Integer
parLsIterComp = do
  _ <- string "iter"
  _ <- space
  i <- parseIntegral
  _ <- parEol
  return (i :: Integer)


parLSysCompSet :: Parser LSysCompSet
parLSysCompSet = do
  _ <- parEol
  l <- parLsLar
  a <- parLsAng
  x <- parLsAxi
  s <- many parLSysComp
  r <- parLsIterComp
  _ <- parEol
  return LSysCompSet {
    lLar = l
    , lAng = a
    , lAxi = x
    , lSeqI = s
    , lIter = r
  }


parseLSysFile :: String -> Either ParseError LSysCompSet
parseLSysFile = parse parLSysCompSet "[Error]"


findRootLSSeq :: LSysCompSet -> Maybe LSLSysComp
findRootLSSeq ls = let
  x = lSeqI ls
  y = readAxiArg $ lAxi ls
  runSeq (r : rs) = if cSaxi r == y
                       then Just r
                    else runSeq rs
  runSeq _ = Nothing
  in runSeq x


makeLSMaps :: LSysCompSet -> M.Map Char [LSLSysSeq]
makeLSMaps ls = let
  mp = M.insert 'B' [LSSubSeq 'B'] $ M.insert 'F' [LSSubSeq 'F'] M.empty
  runSeqs (r : rs) m = let
    ks = cSaxi r
    lv = cSseq r
    nm = M.insert ks lv m
    in runSeqs rs nm
  runSeqs [] m = m
  lxs = lSeqI ls
  in runSeqs lxs mp


remapSubSeq :: LSLSysSeq -> LSLSysSeq
remapSubSeq (LSSubSeq 'B') = LSMoveBackT
remapSubSeq (LSSubSeq 'F') = LSMoveForwardT
remapSubSeq n = n


isSubSeq :: LSLSysSeq -> Bool
isSubSeq (LSSubSeq n) = True
isSubSeq _ = False


notSubSeq :: LSLSysSeq -> Bool
notSubSeq (LSSubSeq n) = False
notSubSeq _ = True


hasSubSeq :: [LSLSysSeq] -> Bool
hasSubSeq xs = length (filter isSubSeq xs) > 0


buildIter :: LSysCompSet
             -> [LSLSysSeq]
buildIter ls = let
  mp = makeLSMaps ls
  ms = lSeqI ls
  ra = readAxiArg $ lAxi ls
  it = fromIntegral (lIter ls) - 1
  mkFinSeq cn (x : xs) am = case x of
    (LSSubSeq n) -> mkFinSeq cn xs $ am ++ (mp M.! n)
    _            -> mkFinSeq cn xs $ am ++ [x]
  mkFinSeq cn [] am = if cn < it
                         then mkFinSeq (cn + 1) am []
                      else am
  in mkFinSeq 1 (mp M.! ra) []


cleanSubSeq :: [LSLSysSeq] -> [LSLSysSeq]
cleanSubSeq = filter notSubSeq


oglDraw :: Turtle -> LSysCompSet -> [LSLSysSeq] -> IO ()
oglDraw t ss ls = let
  ems :: [(Double, Double, Double)]
  ems = []
  oglSDraw :: [LSLSysSeq]
              -> [(Double, Double, Double)]
              -> IO ()
  oglSDraw (x : xs) a = case x of
    LSRotateLeft   -> left t (readAngArg $ lAng ss)
                      >> flush t
                      >> oglSDraw xs a
    LSRotateRight  -> right t (readAngArg $ lAng ss)
                      >> flush t
                      >> oglSDraw xs a
    LSMoveForwardT -> pendown t
                      >> forward t (readLarArg $ lLar ss)
                      >> penup t
                      >> flush t
                      >> oglSDraw xs a
    LSMoveForward  -> penup t
                      >> forward t (readLarArg $ lLar ss)
                      >> flush t
                      >> oglSDraw xs a
    LSMoveBackT    -> pendown t
                      >> backward t (readLarArg $ lLar ss)
                      >> penup t
                      >> flush t
                      >> oglSDraw xs a
    LSMoveBack     -> penup t
                      >> backward t (readLarArg $ lLar ss)
                      >> flush t
                      >> oglSDraw xs a
    LSPushStack    -> do
                      xt <- xcor t
                      yt <- ycor t
                      ht <- heading t
                      oglSDraw xs ((xt, yt, ht) : a)
    LSPopStack     -> let ((nx, ny, nh) : xxs) = a
                      in penup t
                         >> goto t nx ny
                         >> setheading t nh
                         >> oglSDraw xs xxs
    _              -> oglSDraw xs a
  oglSDraw [] a = putStrLn "Done!"
  in penup t
     >> left t 90
     >> shape t "turtle"
     >> shapesize t 2 2
     >> oglSDraw ls ems


mainGlSub :: LSysCompSet -> String -> IO ()
mainGlSub pr f = do
  fld <- openField
  tur <- newTurtle fld
  onkeypress fld $ return . (/= 'q')
  oglDraw tur pr
    $ cleanSubSeq
    $ fmap remapSubSeq
    $ buildIter pr
  saveSVG tur f
  closeField fld


saveSVG :: Turtle -> String -> IO ()
saveSVG t i = getSVG t >>= writeFile (i ++ ".svg") . showSVG 600 600


main :: IO ()
main = do
  [f] <- getArgs
  prg <- readFile f
  case parseLSysFile prg of
    Left err -> print err
    Right pr -> mainGlSub pr f
