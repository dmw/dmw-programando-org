
module Main (main) where


import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import qualified Data.List.Utils as L
import qualified Data.Map as M

import Data.Function
import Data.List
import Data.Maybe ()
import Data.Ord
import Data.String.Utils

import System.Environment
import System.IO
import System.Posix.Temp

import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers
import Text.Printf ()


data LSLarArg = LSLarArg Double deriving (Eq, Show)


data LSAngArg = LSAngArg Double deriving (Eq, Show)


data LSAxiArg = LSAxiArg Char deriving (Eq, Show)


data LSLSysSeq = LSRotateLeft
                 | LSRotateRight
                 | LSMoveForwardT
                 | LSMoveForward
                 | LSMoveBackT
                 | LSMoveBack
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
                LSRotateLeft     -> " - "
                LSRotateRight    -> " + "
                LSMoveForwardT   -> " F "
                LSMoveForward    -> " f "
                LSMoveBackT      -> " B "
                LSMoveBack       -> " b "
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
  x <- oneOf $ ['A' .. 'Z'] ++ ['a' .. 'z']
  _ <- parEol
  return $ LSAxiArg (x :: Char)


parLsSysSeq :: Parser LSLSysSeq
parLsSysSeq = do
  i <- oneOf $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ "+-"
  case i of
       '+' -> return LSRotateRight
       '-' -> return LSRotateLeft
       'f' -> return LSMoveForward
       'F' -> return LSMoveForwardT
       'b' -> return LSMoveBack
       'B' -> return LSMoveBackT
       _   -> return $ LSSubSeq i


parLSysComp :: Parser LSLSysComp
parLSysComp = do
  i <- oneOf "ACDEGHIJKLMNOPQRSTUVWXYZ"
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
  mp = M.empty
  runSeqs (r : rs) m = let
    ks = cSaxi r
    lv = cSseq r
    nm = M.insert ks lv m
    in runSeqs rs nm
  runSeqs [] m = m
  lxs = lSeqI ls
  in runSeqs lxs mp


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
  ra = readAxiArg $ lAxi ls
  it = fromIntegral $ lIter ls
  mkFinSeq cn (x : xs) am = case x of
    (LSSubSeq n) -> mkFinSeq cn xs $ am ++ (mp M.! n)
    _            -> mkFinSeq cn xs $ am ++ [x]
  mkFinSeq cn [] am = if hasSubSeq am && cn < it
                         then mkFinSeq (cn + 1) am []
                      else am
  in mkFinSeq 0 (mp M.! ra) []


cleanSubSeq :: [LSLSysSeq] -> [LSLSysSeq]
cleanSubSeq = filter notSubSeq


oglDraw :: LSysCompSet -> [LSLSysSeq] -> IO ()
oglDraw ss ls = let
  oglSDraw (x : xs) (ppx, ppy) dr = let
    gppx = ppx :: GLfloat
    gppy = ppy :: GLfloat
    (np, ndr) = case x of
                     LSRotateLeft   -> ((0.0, 0.0), False)
                     LSRotateRight  -> ((0.0, 0.0), False)
                     LSMoveForwardT -> ((0.0, 0.0), False)
                     LSMoveForward  -> ((0.0, 0.0), False)
                     LSMoveBackT    -> ((0.0, 0.0), False)
                     LSMoveBack     -> ((0.0, 0.0), False)
    in oglSDraw xs np ndr
  oglSDraw [] (ppx, ppy) dr = putStrLn "Done!"
  in oglSDraw ls (0.0, 0.0) False


mainGlSub :: LSysCompSet -> String -> IO ()
mainGlSub pr f = do
  (progname, _) <- getArgsAndInitialize
  loadIdentity
  initialWindowSize $= Size 540 400
  createWindow $ "L-System " ++ f
  matrixMode $= Projection
  polygonMode $= (Line, Line)
  displayCallback $= oglDraw pr (cleanSubSeq $ buildIter pr)
  mainLoop


-- print iter should change for OpenGL IO ()
main :: IO ()
main = do
  [f] <- getArgs
  prg <- readFile f
  case parseLSysFile prg of
    Left err -> print err
    Right pr -> mainGlSub pr f


