
module Main (main) where


import Data.Maybe ()
import System.Environment
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
                   deriving (Eq, Show)


data LSLSysComp = LSLSysComp {
  c_saxi          :: Char
  , c_sseq        :: [LSLSysSeq]
  } deriving (Eq, Show)


data LSysCompSet = LSysCompSet {
  l_lar           :: LSLarArg
  , l_ang         :: LSAngArg
  , l_axi         :: LSAxiArg
  , l_seqi        :: [LSLSysComp]
  , l_iter        :: Integer
  } deriving (Eq, Show)



parEol :: Parser String
parEol = do
  i <- many $ oneOf ['\n', '\r']
  return $ i


parLsLar :: Parser LSLarArg
parLsLar = do
  _ <- string "lar"
  _ <- space
  x <- parseFloat
  _ <- parEol
  return $ LSLarArg (x :: Double)


parLsAng :: Parser LSAngArg
parLsAng = do
  _ <- string "ang"
  _ <- space
  x <- parseFloat
  _ <- parEol
  return $ LSAngArg (x :: Double)


parLsAxi :: Parser LSAxiArg
parLsAxi = do
  _ <- string "axi"
  _ <- space
  x <- oneOf $ ['A'..'Z'] ++ ['a'..'z']
  _ <- parEol
  return $ LSAxiArg (x :: Char)


parLsSysSeq :: Parser LSLSysSeq
parLsSysSeq = do
  i <- oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['+', '-']
  case i of
       '+' -> return $ LSRotateRight
       '-' -> return $ LSRotateLeft
       'f' -> return $ LSMoveForward
       'F' -> return $ LSMoveForwardT
       'b' -> return $ LSMoveBack
       'B' -> return $ LSMoveBackT
       _   -> return $ LSSubSeq i


parLSysComp :: Parser LSLSysComp
parLSysComp = do
  i <- oneOf "ACDEGHIJKLMNOPQRSTUVWXYZ"
  _ <- char ':'
  s <- many parLsSysSeq
  _ <- parEol
  return LSLSysComp {
    c_saxi = i
    , c_sseq = s
    }


parLsIterComp :: Parser Integer
parLsIterComp = do
  _ <- string "iter"
  _ <- space
  i <- parseIntegral
  _ <- parEol
  return $ (i :: Integer)


parLSysCompSet :: Parser LSysCompSet
parLSysCompSet = do
  _ <- parEol
  l <- parLsLar
  a <- parLsAng
  x <- parLsAxi
  s <- many parLSysComp
  r <- parLsIterComp
  _ <- parEol
  return $ LSysCompSet {
    l_lar = l
    , l_ang = a
    , l_axi = x
    , l_seqi = s
    , l_iter = r
  }


parseLSysFile :: String -> Either ParseError LSysCompSet
parseLSysFile s = parse parLSysCompSet "[Error]" s


parseFile :: String -> IO ()
parseFile s = case (parseLSysFile s) of
                   Left err -> putStrLn $ show err
                   Right pr -> putStrLn $ show pr


main :: IO ()
main = do
  [f] <- getArgs
  prg <- readFile f
  parseFile prg

