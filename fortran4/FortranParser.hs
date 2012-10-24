

module FortranParser
    (
     FPConst (..)
    , FPCmpOp (..)
    , FPAriOp (..)
    , FPGoto (..)
    , FPStop (..)
    , FPRead (..)
    , FPBoolExpr (..)
    , FPArithExpr (..)
    , FPCommonExpr (..)
    , FPAssign (..)
    , FPCommonStm (..)
    , FPIfNStm (..)
    , FPLabelStm (..)
    , FPProgram (..)
    , parseF4Program
    ) where


import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers
import Text.Printf


data FPConst = FPConstInteger Integer
             | FPConstDouble Double
             | FPConstString String
             | FPConstVariable String
               deriving (Eq, Ord)


data FPCmpOp = FPCondEq
               | FPCondLt
               | FPCondGt
               | FPCondLe
               | FPCondGe
                 deriving (Eq, Ord)


data FPAriOp = FPSumOp
             | FPSubOp
             | FPMulOp
             | FPDivOp
             | FPExpOp
               deriving (Eq, Ord)


data FPGoto = FPGoto {
      labelName      :: Integer
      , currentLabel :: Integer
    } deriving (Eq, Ord)


data FPStop = FPStop deriving (Eq, Ord)


data FPRead = FPRead {
      varCount :: Int
    , varSpec  :: Integer
    , varNames :: [String]
    } deriving (Eq, Ord)


data FPBoolExpr = FPCondC FPCmpOp FPConst FPConst
                | FPCondR FPCmpOp FPConst FPBoolExpr
                | FPCondL FPCmpOp FPBoolExpr FPConst
                  deriving (Eq, Ord)


data FPArithExpr = FPOperC FPAriOp FPConst FPConst
                 | FPOperR FPAriOp FPConst FPArithExpr
                 | FPOperL FPAriOp FPArithExpr FPConst
                   deriving (Eq, Ord)


data FPCommonExpr = FPExprA FPArithExpr
                  | FPExprB FPBoolExpr
                  | FPExprC FPConst
                    deriving (Eq, Ord)


data FPAssign = FPAssign {
      varName  :: String
    , varValue :: FPCommonExpr
    } deriving (Eq, Ord)


data FPCommonStm = FPGotoStm FPGoto
                 | FPAssignStm FPAssign
                 | FPIfStm FPIfNStm
                 | FPLblStm FPLabelStm
                 | FPRdStm FPRead
                 | FPCommentStm
                 | FPStopStm FPStop
                   deriving (Eq, Ord)


data FPIfNStm = FPIfNStm {
      boolExpr :: FPBoolExpr
    , stmList  :: [FPCommonStm]
    } deriving (Eq, Ord)


data FPLabelStm = FPLabelStm {
      labelStm   :: Integer
    , labeledStm :: [FPCommonStm]
    } deriving (Eq, Ord)


data FPProgram = FPProgram {
      readStm :: FPCommonStm
    , initStm :: [FPCommonStm]
    , progStm :: [FPCommonStm]
    } deriving (Eq, Ord, Show)


fparIdentifier :: Parser String
fparIdentifier = many1 $ oneOf (['A' .. 'Z'] ++ ['a' .. 'z'] ++ "_")


fparString :: Parser String
fparString = do
  _ <- char '"'
  s <- many $ noneOf "\r\n\""
  _ <- char '"'
  return s


readEol :: Parser String
readEol = many1 $ oneOf "\r\n"


fparComment :: Parser FPCommonStm
fparComment = char 'C'
              >> many1 (noneOf "\r\n")
              >> readEol
              >> return FPCommentStm


fparVarConstInt :: Parser FPConst
fparVarConstInt = do
  i <- parseIntegral
  return $ FPConstInteger i


fparVarConstFlt :: Parser FPConst
fparVarConstFlt = do
  i <- parseFloat
  return $ FPConstDouble i


fparVarConstStr :: Parser FPConst
fparVarConstStr = do
  i <- fparString
  return $ FPConstString i


fparVarConstVar :: Parser FPConst
fparVarConstVar = do
  i <- fparIdentifier
  return $ FPConstVariable i


fparVarConstP :: Parser FPConst
fparVarConstP = try fparVarConstInt
                <|> try fparVarConstFlt
                <|> try fparVarConstStr
                <|> fparVarConstVar


fparBoolOper :: Parser FPCmpOp
fparBoolOper = do
  o <- try (string ".le.")
       <|> try (string ".ge.")
       <|> try (string ".lt.")
       <|> try (string ".gt.")
       <|> string ".eq."
  case o of
    ".lt." -> return FPCondLt
    ".gt." -> return FPCondGt
    ".le." -> return FPCondLe
    ".ge." -> return FPCondGe
    ".eq." -> return FPCondEq
    _      -> fail $ "Error Code:" ++ show o


fparArithOper :: Parser FPAriOp
fparArithOper = do
  o <- try (string "^")
       <|> try (string "*")
       <|> try (string "/")
       <|> try (string "+")
       <|> string "-"
  case o of
    "+" -> return FPSumOp
    "-" -> return FPSubOp
    "*" -> return FPMulOp
    "/" -> return FPDivOp
    "^" -> return FPExpOp
    _      -> fail $ "Error Code:" ++ show o


fparBoolOperExpr :: Parser FPCmpOp
fparBoolOperExpr = do
  _ <- many space
  o <- fparBoolOper
  _ <- many space
  return o


fparConstBoolExpr :: Parser FPBoolExpr
fparConstBoolExpr = do
  c <- try fparVarConstP
  o <- fparBoolOperExpr
  d <- fparVarConstP
  return $ FPCondC o c d


fparLeftBoolExpr :: Parser FPBoolExpr
fparLeftBoolExpr = do
  c <- fparConstBoolExpr
  o <- fparBoolOperExpr
  d <- fparVarConstP
  return $ FPCondL o c d


fparRightBoolExpr :: Parser FPBoolExpr
fparRightBoolExpr = do
  c <- fparVarConstP
  o <- fparBoolOperExpr
  d <- fparConstBoolExpr
  return $ FPCondR o c d


fparBoolExpr :: Parser FPBoolExpr
fparBoolExpr = try fparConstBoolExpr
               <|> try fparLeftBoolExpr
               <|> fparRightBoolExpr


fparArithOperExpr :: Parser FPAriOp
fparArithOperExpr = do
  _ <- many space
  o <- fparArithOper
  _ <- many space
  return o


fparConstArithExpr :: Parser FPArithExpr
fparConstArithExpr = do
  c <- fparVarConstP
  o <- fparArithOperExpr
  d <- fparVarConstP
  return $ FPOperC o c d


fparLeftArithExpr :: Parser FPArithExpr
fparLeftArithExpr = do
  c <- fparConstArithExpr
  o <- fparArithOperExpr
  d <- fparVarConstP
  return $ FPOperL o c d


fparRightArithExpr :: Parser FPArithExpr
fparRightArithExpr = do
  c <- fparVarConstP
  o <- fparArithOperExpr
  d <- fparConstArithExpr
  return $ FPOperR o c d


fparArithExpr :: Parser FPArithExpr
fparArithExpr = try fparConstArithExpr
                <|> try fparLeftArithExpr
                <|> fparRightArithExpr


fparAssignExprConst :: Parser FPAssign
fparAssignExprConst = do
  i <- fparIdentifier
  _ <- many space >> char '=' >> many space
  e <- fparVarConstP
  return FPAssign { varName = i, varValue = FPExprC e }


fparAssignExprArith :: Parser FPAssign
fparAssignExprArith = do
  i <- fparIdentifier
  _ <- many space >> char '=' >> many space
  e <- fparArithExpr
  return FPAssign { varName = i, varValue = FPExprA e }


fparAssignExprBool :: Parser FPAssign
fparAssignExprBool = do
  i <- fparIdentifier
  _ <- many space >> char '=' >> many space
  e <- fparBoolExpr
  return FPAssign { varName = i, varValue = FPExprB e }


fparAssignExpr :: Parser FPCommonStm
fparAssignExpr = do
  e <- try fparAssignExprBool
       <|> try fparAssignExprArith
       <|> fparAssignExprConst
  return $ FPAssignStm e


fparStmGoto :: Integer -> Parser FPCommonStm
fparStmGoto o = do
  _ <- string "goto"
  _ <- many space
  l <- parseIntegral
  return $ FPGotoStm FPGoto { labelName = l, currentLabel = o }


fparIfExpr :: Integer -> Parser FPCommonStm
fparIfExpr l = do
  _ <- string "if"
  _ <- many space
  _ <- char '('
  _ <- many space
  e <- fparBoolExpr
  _ <- many space
  _ <- char ')'
  _ <- many space
  r <- try (fparStmGoto l) <|> fparAssignExpr
  return $ FPIfStm FPIfNStm { boolExpr = e, stmList = [r] }


fparStop :: Parser FPCommonStm
fparStop = do
  _ <- many space
  _ <- string "stop"
  return $ FPStopStm FPStop


fparCommonStm :: Integer -> Parser FPCommonStm
fparCommonStm l = do
  _ <- many space
  x <- try (fparIfExpr l)
       <|> try (fparStmGoto l)
       <|> try fparStop
       <|> fparAssignExpr
  _ <- readEol
  return x


fparLabelStm :: Parser FPCommonStm
fparLabelStm = do
  _ <- many space
  i <- parseIntegral
  e <- manyTill (fparCommonStm i)
       (try (optional
             $ lookAhead
             $ many space >> parseIntegral))
  return $ FPLblStm FPLabelStm {
                 labelStm = i
               , labeledStm = e
               }


fparHeader :: Parser FPCommonStm
fparHeader = do
  _ <- many space
  _ <- string "read"
  _ <- many space
  i <- parseIntegral
  _ <- many (oneOf ", ")
  e <- fparIdentifier `sepBy` many (oneOf ", ")
  _ <- readEol
  return $ FPRdStm FPRead {
               varCount = length e
             , varSpec = i
             , varNames = e
             }


fparProgram :: Parser FPProgram
fparProgram = do
  _ <- many fparComment
  h <- fparHeader
  i <- manyTill (fparCommonStm 0)
       (try (optional
             $ lookAhead
             $ many space >> parseIntegral))
  l <- many fparLabelStm
  _ <- optional fparStop
  _ <- optional eof
  return FPProgram {
                readStm = h
              , initStm = i
              , progStm = l
              }


parseF4Program :: String -> Either ParseError FPProgram
parseF4Program = parse fparProgram "[Error]"


instance Show FPCommonStm where
    show (FPGotoStm x)   = printf "goto: %d -> %d" (labelName x) (currentLabel x)
    show (FPAssignStm x) = printf "set: %s <- %s" (varName x) (show $ varValue x)
    show (FPIfStm x)     = printf "if (%s)" (show $ boolExpr x)
    show (FPLblStm x)    = printf "label: %d" (labelStm x)
    show (FPRdStm x)     = printf "read: %d, vars: %s" (varSpec x) (show $ varNames x)
    show (FPStopStm _)   = printf "stop"
    show (FPCommentStm)  = printf "comment"


instance Show FPBoolExpr where
    show (FPCondC o a b) = printf "%s %s %s" (show a) (show o) (show b)
    show (FPCondR o a b) = printf "%s %s (%s)" (show a) (show o) (show b)
    show (FPCondL o a b) = printf "(%s) %s %s" (show a) (show o) (show b)


instance Show FPArithExpr where
    show (FPOperC o a b) = printf "%s %s %s" (show a) (show o) (show b)
    show (FPOperR o a b) = printf "%s %s (%s)" (show a) (show o) (show b)
    show (FPOperL o a b) = printf "(%s) %s %s" (show a) (show o) (show b)


instance Show FPCmpOp where
    show FPCondEq = "=="
    show FPCondLt = "<"
    show FPCondGt = ">"
    show FPCondLe = "<="
    show FPCondGe = ">="


instance Show FPAriOp where
    show FPSumOp = "+"
    show FPSubOp = "-"
    show FPMulOp = "*"
    show FPDivOp = "/"
    show FPExpOp = "^"


instance Show FPConst where
    show (FPConstInteger x)  = show x
    show (FPConstDouble x)   = show x
    show (FPConstString x)   = printf "string: \"%s\"" x
    show (FPConstVariable x) = printf "var: %s" x


instance Show FPCommonExpr where
    show (FPExprA x) = show x
    show (FPExprB x) = show x
    show (FPExprC x) = show x


instance Show FPAssign where
    show x = printf "%s <- %s" (varName x) (show $ varValue x)


instance Show FPRead where
    show x = printf "read %s; vars: %s" (show $ varSpec x) (show $ varNames x)


instance Show FPGoto where
    show x = printf "goto: %d -> %d" (show $ labelName x) (show $ currentLabel x)


instance Show FPStop where
    show _ = "stop"


instance Show FPIfNStm where
    show x = printf "if (%s)" (show $ boolExpr x)


instance Show FPLabelStm where
    show x = printf "label: %s" (show $ labelStm x)


-- end module
