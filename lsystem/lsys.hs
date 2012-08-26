
module Main (main) where



import Graphics.X11.Turtle

import Control.Monad
import Control.Concurrent

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
import System.Random

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
                LSRotateLeft     -> " Izq "
                LSRotateRight    -> " Der "
                LSMoveForwardT   -> " AvaT "
                LSMoveForward    -> " Ava "
                LSMoveBackT      -> " RetT "
                LSMoveBack       -> " Ret "
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
       '-' -> return LSRotateRight
       '+' -> return LSRotateLeft
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
  ms = lSeqI ls
  ra = readAxiArg $ lAxi ls
  it = fromIntegral $ lIter ls
  mkFinSeq cn (x : xs) am = case x of
    (LSSubSeq n) -> mkFinSeq cn xs $ am ++ (mp M.! n)
    _            -> mkFinSeq cn xs $ am ++ [x]
  mkFinSeq cn [] am = if cn < it
                         then mkFinSeq (cn + 1) am []
                      else am
  in mkFinSeq 0 (mp M.! ra) []


cleanSubSeq :: [LSLSysSeq] -> [LSLSysSeq]
cleanSubSeq = filter notSubSeq


oglDraw :: Turtle -> LSysCompSet -> [LSLSysSeq] -> IO ()
oglDraw t ss ls = let
  oglSDraw (x : xs) = case x of
    LSRotateLeft   -> (left t $ readAngArg $ lAng ss)
                      >> flush t
                      >> sleep t 10
                      >> oglSDraw xs
    LSRotateRight  -> (right t $ readAngArg $ lAng ss)
                      >> flush t
                      >> sleep t 10
                      >> oglSDraw xs
    LSMoveForwardT -> pendown t
                      >> (forward t $ readLarArg $ lLar ss)
                      >> penup t
                      >> flush t
                      >> sleep t 10
                      >> oglSDraw xs
    LSMoveForward  -> penup t
                      >> (forward t $ readLarArg $ lLar ss)
                      >> flush t
                      >> sleep t 10
                      >> oglSDraw xs
    LSMoveBackT    -> pendown t
                      >> (backward t $ readLarArg $ lLar ss)
                      >> penup t
                      >> flush t
                      >> sleep t 10
                      >> oglSDraw xs
    LSMoveBack     -> penup t
                      >> (backward t $ readLarArg $ lLar ss)
                      >> flush t
                      >> sleep t 10
                      >> oglSDraw xs
    _              -> oglSDraw xs
  oglSDraw [] = putStrLn "Done!" >> sleep t 10
  in do
     penup t
     shape t "horse"
     shapesize t 2 2
     oglSDraw ls


mainGlSub :: LSysCompSet -> String -> IO ()
mainGlSub pr f = do
  f <- openField
  t <- newTurtle f
  onkeypress f $ return . (/= 'q')
  oglDraw t pr $ cleanSubSeq $ buildIter pr
  threadDelay 10000000
  closeField f


main :: IO ()
main = do
  [f] <- getArgs
  prg <- readFile f
  case parseLSysFile prg of
    Left err -> print err
    Right pr -> putStrLn (show pr) >> mainGlSub pr f

