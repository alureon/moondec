module Text.Lua where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Language

data Expression = Lit Double
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Mod Expression Expression
                | Neg Expression
                | Len Expression
                | Not Expression
                | Boolean Bool
                | Eq Expression Expression
                | Neq Expression Expression
                | Pow Expression Expression
                | Cat Expression Expression
                | Lt Expression Expression
                | Gt Expression Expression
                | Lte Expression Expression
                | Gte Expression Expression
                | And Expression Expression
                | Or Expression Expression
                | Nil
                deriving (Show)

data Statement = GlobalAssign [String] [Expression]
               | LocalAssign [String] [Expression]
               | IfStatement Expression [Statement]
               | WhileStatement Expression [Statement]
               | DoStatement [Statement]
               deriving (Show)

lexer :: TokenParser ()
lexer = makeTokenParser (emptyDef
    { commentStart = "--"
    , commentEnd = "\n"
    , identStart = letter
    , identLetter = alphaNum
    , opStart  = oneOf "+-*/%notadr"
    , opLetter = oneOf "+-*/%notadr"
    , reservedOpNames = ["and", "or"]
    })

parseComment :: Parser ()
parseComment = do
    string "--"
    many $ noneOf "\n"
    endOfLine
    return ()

parseNumber, parseBoolean, parseNil :: Parser Expression

parseNumber = do
    v <- naturalOrFloat lexer
    case v of
        Left i -> return $ Lit $ fromIntegral i
        Right n -> return $ Lit n

parseBoolean = do
    v <- string "true" <|> string "false"
    whiteSpace lexer
    return $ case v of
        "true" -> Boolean True
        "false" -> Boolean False

parseNil = do
    reserved lexer "nil"
    return Nil

makeExprParser = flip buildExpressionParser

parseExpression :: Parser Expression
parseExpression = makeExprParser parseTerm $ [
        [ Prefix (reservedOp lexer "-" >> return Neg)
        , Prefix (reservedOp lexer "#" >> return Len)
        , Prefix (reservedOp lexer "not" >> return Not) ]
      , [ Infix (reservedOp lexer "*" >> return Mul) AssocLeft
        , Infix (reservedOp lexer "/" >> return Div) AssocLeft
        , Infix (reservedOp lexer "%" >> return Mod) AssocLeft ]
      , [ Infix (reservedOp lexer "+" >> return Add) AssocLeft
        , Infix (reservedOp lexer "-" >> return Sub) AssocLeft
        , Infix (reservedOp lexer "^" >> return Pow) AssocLeft
        , Infix (reservedOp lexer ".." >> return Cat) AssocLeft ]
      , [ Infix (reservedOp lexer "==" >> return Eq) AssocLeft
        , Infix (reservedOp lexer "~=" >> return Neq) AssocLeft
        , Infix (reservedOp lexer "<" >> return Lt) AssocLeft
        , Infix (reservedOp lexer ">" >> return Gt) AssocLeft
        , Infix (reservedOp lexer "<=" >> return Lte) AssocLeft
        , Infix (reservedOp lexer ">=" >> return Gte) AssocLeft
        , Infix (reservedOp lexer "and" >> return And) AssocLeft
        , Infix (reservedOp lexer "or" >> return Or) AssocLeft ]
    ]

parseStatement, parseIfStatement, parseDoStatement,
    parseWhileStatement :: Parser Statement

parseIfStatement = do
    reserved lexer "if"
    cond <- parseExpression
    reserved lexer "then"
    s <- many parseStatement
    reserved lexer "end"
    return $ IfStatement cond s

parseWhileStatement = do
    reserved lexer "while"
    cond <- parseExpression
    reserved lexer "do" -- could we make this a parseDoStatement?
    s <- many parseStatement
    reserved lexer "end"
    return $ WhileStatement cond s

parseDoStatement = do
    reserved lexer "do"
    s <- many parseStatement
    reserved lexer "end"
    return $ DoStatement s

parseNameList :: Parser [String]
parseNameList = try $ (identifier lexer) `sepBy` (char ',' >> whiteSpace lexer) <|> count 1 (identifier lexer)

parseExpressionList :: Parser [Expression]
parseExpressionList = try $ parseExpression `sepBy` (char ',' >> whiteSpace lexer) <|> count 1 parseExpression

parseLocalAssignStatement :: Parser Statement
parseLocalAssignStatement = do
    reserved lexer "local"
    s <- parseNameList
    e <- option [] $ whiteSpace lexer >> char '=' >> whiteSpace lexer >> parseExpressionList
    return $ LocalAssign s e

parseStatement = do
    s <- try parseIfStatement <|> try parseWhileStatement
        <|> try parseLocalAssignStatement
    skipMany $ char ';'
    return s

parseTerm :: Parser Expression
parseTerm = parens lexer parseExpression <|> parseNumber <|> parseBoolean
    <|> parseNil

