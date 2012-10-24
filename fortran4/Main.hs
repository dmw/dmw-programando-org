
module Main (main) where

import System.Console.GetOpt
import System.Exit
import System.IO
import System.Environment
import System.IO.Unsafe ()
import System.Posix.Temp ()
import Text.Printf

import qualified FortranParser as FP
import qualified FortranAST as FA


data MainOptions = MainOptions {
      inputFileA :: String
    , inputFileB :: String
    , procTypeFG :: String
    , procTypeSL :: String
    , beVerbose  :: Bool
    } deriving (Eq, Ord, Show)


progVersion :: String
progVersion = "Fortran IV Analyzer 0.0.1"


startOptions :: MainOptions
startOptions = MainOptions {
                 inputFileA = "example1.f90"
               , inputFileB = "example2.f90"
               , procTypeFG = "none"
               , procTypeSL = "example1.f90"
               , beVerbose  = False
               }

progOptions :: [OptDescr (MainOptions -> IO MainOptions)]
progOptions =
  [ Option "a" ["input_a"]
    (ReqArg (\ x o -> return o { inputFileA = x }) "FILE_A")
    "input file A"
  , Option "b" ["input_b"]
    (ReqArg (\ x o -> return o { inputFileB = x }) "FILE_B")
    "input file B"
  , Option "x" ["proc"]
    (ReqArg (\ x o -> return o { procTypeFG = x }) "PROC_G")
    "processing type file (supported: x11, print, save)"
  , Option "s" ["select"]
    (ReqArg (\ x o -> return o { procTypeSL = x }) "PROC_S")
    "processing selection file"
  , Option "v" ["verbose"]
    (NoArg (\ o -> return o { beVerbose = True }))
    "verbose output"
  , Option "V" ["version"]
    (NoArg (\ _ -> hPutStrLn stderr progVersion
                   >> exitSuccess))
    "displays program version"
  , Option "h" ["help"]
    (NoArg (\ _ -> do
                   prg <- getProgName
                   hPutStrLn stderr (usageInfo prg progOptions)
                   >> exitSuccess))
    "displays this message"
  ]



handlerIOError :: IOError -> IO ()
handlerIOError e = putStrLn (printf "IOError: %s" $ show e)
                   >> exitFailure


procOption :: String -> String -> String -> IO ()
procOption pc inpData inpF = do
  case pc of
    "x11"   -> do putStrLn $ printf "/// Processing: %s" inpF
                  FA.plotFortranAST $ FP.parseF4Program inpData
    "print" -> do putStrLn $ printf "/// Processing: %s" inpF
                  FA.printFortranAST $ FP.parseF4Program inpData
    "save"  -> do putStrLn $ printf "/// Processing: %s" inpF
                  FA.saveFortranAST (inpF ++ ".png") $ FP.parseF4Program inpData
    _        -> putStrLn $ "/// Valid Processing Options: x11, print; Got: " ++ pc
  putStrLn $ "/// Got: " ++ pc


processArgs :: IO ()
processArgs = do
    argv <- getArgs
    let (act, nopt, errs) = getOpt RequireOrder progOptions argv
    opts <- foldl (>>=) (return startOptions) act
    inpDataA <- readFile (inputFileA opts)
    inpDataB <- readFile (inputFileB opts)
    if procTypeSL opts == inputFileA opts
       then procOption (procTypeFG opts) inpDataA (inputFileA opts)
    else procOption (procTypeFG opts) inpDataB (inputFileB opts)


main :: IO ()
main = processArgs `catch` handlerIOError
