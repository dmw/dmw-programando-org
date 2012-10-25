{-# LANGUAGE ScopedTypeVariables #-}

module FortranAST
    (
     plotFortranAST
    , printFortranAST
    , saveFortranAST
    , getFortranAST
    )where


import Control.Concurrent
import Control.Arrow ()
import Data.Graph.Inductive
import Data.Graph.Inductive.Graph ()
import Data.GraphViz
import Data.GraphViz.Attributes ()
import Data.GraphViz.Attributes.Complete
import Text.Printf
import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers ()

import qualified FortranParser as FP


data FortranGrData = FortranGrData {
      fortranNodes :: [LNode FP.FPCommonStm]
    , fortranEdges :: [LEdge String]
    , currentIndex :: Int
    , currentParen :: [Int]
    } deriving (Eq, Ord, Show)


emptyGrData :: FortranGrData
emptyGrData = FortranGrData {
                fortranNodes = []
              , fortranEdges = []
              , currentIndex = 1
              , currentParen = []
              }


getNodeLabeled :: Int -> FP.FPCommonStm -> LNode FP.FPCommonStm
getNodeLabeled y m = (y, m)


getEdgeLabeled :: Int -> Int -> LEdge String
getEdgeLabeled y y' = (y, y', printf "%d &rarr; %d" y y')


getRootFPNode :: FP.FPProgram -> FortranGrData -> FortranGrData
getRootFPNode p g = g {
                      fortranNodes = getNodeLabeled 1 (FP.readStm p) : fortranNodes g
                    , fortranEdges = []
                    , currentIndex = 1
                    , currentParen = [1]
                    }


appendNode' :: FortranGrData -> LNode FP.FPCommonStm -> LEdge String -> Int -> FortranGrData
appendNode' r cn en nc = let
    ps = fortranNodes r ++ [cn]
    se = fortranEdges r ++ [en]
    nr = r { currentIndex = nc
           , fortranNodes = ps
           , fortranEdges = se
           , currentParen = nc : currentParen r
           }
    in nr


procNodes :: [FP.FPCommonStm]
          -> FortranGrData
          -> FortranGrData
procNodes ts g = let
    procNodes' :: [FP.FPCommonStm]
               -> FortranGrData
               -> FortranGrData
    procNodes' (FP.FPLblStm x:xs) r = let
                        sn = FP.labeledStm x
                        pc = head $ currentParen r
                        nc = currentIndex r + 1
                        cn = getNodeLabeled nc (FP.FPLblStm x)
                        en = getEdgeLabeled pc nc
                        nr = appendNode' r cn en nc
                        np = procNodes' sn nr
                        lr = np { currentParen = drop 1 $ currentParen np }
                        in procNodes' xs lr
    procNodes' (FP.FPIfStm x:xs) r = let
                        sn = FP.stmList x
                        pc = head $ currentParen r
                        nc = currentIndex r + 1
                        cn = getNodeLabeled nc (FP.FPIfStm x)
                        en = getEdgeLabeled pc nc
                        nr = appendNode' r cn en nc
                        np = procNodes' sn nr
                        lr = np { currentParen = drop 1 $ currentParen np }
                        in procNodes' xs lr
    procNodes' (x:xs) r = let
                        pc = head $ currentParen r
                        nc = currentIndex r + 1
                        cn = getNodeLabeled nc x
                        en = getEdgeLabeled pc nc
                        ps = fortranNodes r ++ [cn]
                        se = fortranEdges r ++ [en]
                        nr = r { currentIndex = nc
                               , fortranNodes = ps
                               , fortranEdges = se
                               }
                        in procNodes' xs nr
    procNodes' [] r = r
    in procNodes' ts g


getFreeFPNodes :: [FP.FPCommonStm] -> FortranGrData -> FortranGrData
getFreeFPNodes = procNodes


getFortranAST :: FP.FPProgram -> FortranGrData
getFortranAST p = let
    rg = getRootFPNode p emptyGrData
    fn = getFreeFPNodes (FP.initStm p) rg
    rn = getFreeFPNodes (FP.progStm p) fn
    in rn


gStyle :: [GlobalAttributes]
gStyle = [ GraphAttrs [RankDir FromLeft, Splines SplineEdges]
         , NodeAttrs  [shape Note, fontColor Blue]
         , EdgeAttrs  [color Black, style solid, arrowFrom noArrow, arrowTo normal]
         ]


previewGraph' :: (Ord el, Graph gr, Labellable nl, Labellable el)
                => gr nl el
                -> IO ()
previewGraph' g = ign $ forkIO (ign $ runGraphvizCanvas' dg Xlib)
  where
    dg = setDirectedness graphToDot params g
    params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel l]
                                , fmtEdge = \ (_, _, l) -> [toLabel l]
                                , isDirected = True
                                , globalAttributes = gStyle
                                }
    ign = (>> return ())



plotFortranAST :: Either ParseError FP.FPProgram -> IO ()
plotFortranAST x = case x of
                     Left err -> print err
                     Right pr -> let gr = getFortranAST pr
                                     ng :: Gr FP.FPCommonStm String
                                     ng = mkGraph (fortranNodes gr) (fortranEdges gr)
                                 in print ng >> previewGraph' ng


printFortranAST :: Either ParseError FP.FPProgram -> IO ()
printFortranAST x = case x of
                      Left err -> print err
                      Right pr -> let gr = getFortranAST pr
                                      ng :: Gr FP.FPCommonStm String
                                      ng = mkGraph (fortranNodes gr) (fortranEdges gr)
                                  in print ng


saveFortranAST :: FilePath -> Either ParseError FP.FPProgram -> IO ()
saveFortranAST fn x = case x of
                        Right pr -> let gr = getFortranAST pr
                                        ng :: Gr FP.FPCommonStm String
                                        ng = mkGraph (fortranNodes gr) (fortranEdges gr)
                                    in print ng
                                       >> graphToDotPng fn ng
                                       >> putStrLn "/// Saved..."
                        _        -> putStrLn "/// Error"
    where graphToDotPng :: FilePath -> Gr FP.FPCommonStm String -> IO FilePath
          graphToDotPng fpre g = runGraphviz (graphToDot params g) Png fpre
          params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel l]
                                      , fmtEdge = \ (_, _, l) -> [toLabel l]
                                      , isDirected = True
                                      , globalAttributes = gStyle
                                      }
