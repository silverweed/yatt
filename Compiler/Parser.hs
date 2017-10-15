{-| Simple recursive descent parser -}
module Compiler.Parser where

import Prelude hiding (lex)
import Compiler.Lexer hiding (makeTestCase)
import Test.HUnit

{- program : statement
 - statement : block | decllist ';' | expression ';'
 - block : '{' statements '}'
 - statements : statement statements | empty
 - expression : primary binoprhs
 - primary : identifierexpr | numberexpr | '(' expression ')'
 - binoprhs : binop primary | empty
 - numberexpr : NUM
 - binop : '+'
 - decllist : declident declaration declarations
 - declident : 'val'
 - declarations : ',' declaration
 - declaration : id '=' expression
 -}
data Program = Program Statement deriving (Eq, Show)

data Statement = BlockStmt [Statement]
               | ExprStmt  Expression
               | DeclStmt  DeclList
               deriving (Eq, Show)

data Expression = Expression PrimaryExpr [BinOpRHS]
                deriving (Eq, Show)

data DeclList = DeclList DeclType [Declaration]
              deriving (Eq, Show)

data DeclType = DeclVal deriving Eq
instance Show DeclType where
    show DeclVal = "DeclVal"

data Declaration = Declaration String Expression
                 deriving (Eq, Show)

data PrimaryExpr = IdentExpr  String
                 | NumberExpr NumberType
                 | ParenExpr  Expression
                 deriving (Eq, Show)

data BinOpRHS = BinOpRHS BinOp PrimaryExpr deriving (Eq, Show)

data BinOp = OpPlus deriving Eq

instance Show BinOp where
    show OpPlus = "OpPlus"

type NumberType = Double

parse :: [Token] -> Program
parse = parseProgram

parseProgram :: [Token] -> Program
parseProgram tokens = let (stmt, rest) = parseStatement tokens in
                          case rest of
                          [] -> Program stmt
                          _  -> error $ "Syntax error: extra tokens " ++ (show rest) ++ " at the end of the program"

-- parses a single statement and returns (parsed statement, tokens left)
parseStatement :: [Token] -> (Statement, [Token])
parseStatement tokens@(t:toks)
    | t == T_CURLY_L = parseBlock toks
    | t == T_VAL     = let (decl, rest) = parseDecllist tokens in
                           case length rest > 0 && head rest == T_SEMICOLON of
                               True  -> (DeclStmt decl, tail rest)
                               False -> error $ "Missing ';' at the end of statement! (have " ++ (show rest) ++ ")"
    | otherwise      = let (expr, rest) = parseExpression tokens in
                           case length rest > 0 && head rest == T_SEMICOLON of
                               True  -> (ExprStmt expr, tail rest)
                               False -> error $ "Missing ';' at the end of statement! (have " ++ (show rest) ++ ")"

-- parses a block starting from the token after the beginning '{'
parseBlock :: [Token] -> (Statement, [Token])
parseBlock toks = let (stmts, r:rest) = parseStatements toks in
                      case r of
                          T_CURLY_R -> (BlockStmt stmts, rest)
                          _ -> error $ "Missing '}' at the end of block! (have " ++ (show r) ++ ")"

parseStatements :: [Token] -> ([Statement], [Token])
parseStatements tokens@(t:toks) | t == T_CURLY_R = ([], tokens)
                                | otherwise      = (stmt:stmts, rests)
                                                   where
                                                   (stmt, rest) = parseStatement tokens
                                                   (stmts, rests) = parseStatements rest

parseDecllist :: [Token] -> (DeclList, [Token])
parseDecllist (t:toks) | t == T_VAL = (DeclList DeclVal $ decl:decls, rest2)
                       | otherwise  = error $ "Unknown declident " ++ (show t)
                       where
                       (decl, rest)   = parseDeclaration toks
                       (decls, rest2) = parseDeclarations rest

parseDeclaration :: [Token] -> (Declaration, [Token])
parseDeclaration ((T_IDENT id):T_ASSIGN:toks) = let (expr, rest) = parseExpression toks in
                                                    (Declaration id expr, rest)
parseDeclaration (t:_) = error $ "Error in declaration: " ++ (show t) ++ " is not a valid identifier."

parseDeclarations :: [Token] -> ([Declaration], [Token])
parseDeclarations (T_COMMA:toks) = (decl : decls, rest2)
                                   where
                                   (decl, rest)   = parseDeclaration toks
                                   (decls, rest2) = parseDeclarations rest
parseDeclarations toks = ([], toks)

parseExpression :: [Token] -> (Expression, [Token])
parseExpression tokens = (Expression lhs rhs, rest2)
                         where
                         (lhs, rest)  = parseUnary tokens
                         (rhs, rest2) = parseBinOpRHS rest

parseUnary :: [Token] -> (PrimaryExpr, [Token])
parseUnary = parsePrimary -- TODO

parsePrimary :: [Token] -> (PrimaryExpr, [Token])
parsePrimary (T_PAREN_L:toks)    = parseParenExpr toks
parsePrimary ((T_NUM n):toks)    = (NumberExpr n, toks)
parsePrimary ((T_IDENT id):toks) = (IdentExpr id, toks)
parsePrimary (other:toks)        = error $ "Expected primary expression, not '" ++ (show other) ++ "'"

parseParenExpr :: [Token] -> (PrimaryExpr, [Token])
parseParenExpr tokens = let (expr, r:rest) = parseExpression tokens in
                            case r of
                                T_PAREN_R -> (ParenExpr $ tryReduce expr, rest)
                                _ -> error "Missing ')' at the end of parens expression!"
                            where
                            -- Reduce ((expr)) to (expr)
                            tryReduce :: Expression -> Expression
                            tryReduce (Expression (ParenExpr expr) _) = expr
                            tryReduce expr = expr

-- binoprhs : binop primary binoprhs | empty
parseBinOpRHS :: [Token] -> ([BinOpRHS], [Token])
parseBinOpRHS [] = ([], [])
parseBinOpRHS tokens@(t:toks) = case parseBinOp t of
                                    Just binop -> ((BinOpRHS binop expr) : rhss, rests)
                                                  where
                                                  (expr, rest) = parsePrimary toks
                                                  (rhss, rests) = parseBinOpRHS rest
                                    Nothing -> ([], tokens)


parseBinOp :: Token -> Maybe BinOp
parseBinOp T_PLUS = Just OpPlus
parseBinOp _ = Nothing

parseNum :: Token -> PrimaryExpr
parseNum (T_NUM n) = NumberExpr n
parseNum t = error $ "Not a number: " ++ (show t)

------------------------------------------------ TESTS

testParseNum = TestCase (assertEqual "num 1" (NumberExpr 1.0) (parseNum (T_NUM 1.0)))
testParseExpr = TestList
    [ makeTestCase "expr" -- 1
          (Expression (NumberExpr 1.0) [], [])
          parseExpression [T_NUM 1.0]

    , makeTestCase "expr" -- 1 + 2
          (Expression (NumberExpr 1.0) [BinOpRHS OpPlus (NumberExpr 2.0)], [])
          parseExpression [T_NUM 1.0, T_PLUS, T_NUM 2.0]

    , makeTestCase "expr" -- ( 3 )
          (Expression (ParenExpr (Expression (NumberExpr 3.0) [])) [], [])
          parseExpression [T_PAREN_L, T_NUM 3.0, T_PAREN_R]

    , makeTestCase "expr" -- ( ( ( ( 3 ) ) ) )
          (Expression (ParenExpr (Expression (NumberExpr 3.0) [])) [], [])
          parseExpression [T_PAREN_L, T_PAREN_L, T_PAREN_L, T_PAREN_L, T_NUM 3.0,
                            T_PAREN_R, T_PAREN_R, T_PAREN_R, T_PAREN_R]

    , makeTestCase "expr" -- 1 + 2 + 3
          (Expression (NumberExpr 1.0) [BinOpRHS OpPlus (NumberExpr 2.0), BinOpRHS OpPlus (NumberExpr 3.0)], [])
          parseExpression [T_NUM 1.0, T_PLUS, T_NUM 2.0, T_PLUS, T_NUM 3.0]

    , makeTestCase "expr" -- 1 + 2 + 3 + 4 + 5
          (Expression (NumberExpr 1.0)
              [ BinOpRHS OpPlus (NumberExpr 2.0)
              , BinOpRHS OpPlus (NumberExpr 3.0)
              , BinOpRHS OpPlus (NumberExpr 4.0)
              , BinOpRHS OpPlus (NumberExpr 5.0)
              ], [])
          parseExpression [T_NUM 1.0, T_PLUS, T_NUM 2.0, T_PLUS, T_NUM 3.0, T_PLUS, T_NUM 4.0, T_PLUS, T_NUM 5.0]

    , makeTestCase "expr" -- ( 1 + 2 ) + 3
          (Expression
              (ParenExpr (Expression (NumberExpr 1.0) [BinOpRHS OpPlus (NumberExpr 2.0)]))
              [BinOpRHS OpPlus (NumberExpr 3.0)],
          [])
          parseExpression [T_PAREN_L, T_NUM 1.0, T_PLUS, T_NUM 2.0, T_PAREN_R, T_PLUS, T_NUM 3.0]

    , makeTestCase "expr" -- ( 1 + ( 2 + 3 ) )
          (Expression
              (ParenExpr (Expression (NumberExpr 1.0)
                  [ BinOpRHS OpPlus (ParenExpr (Expression (NumberExpr 2.0) [BinOpRHS OpPlus (NumberExpr 3.0)]))
                  ]))
              [],
          [])
          parseExpression [T_PAREN_L, T_NUM 1.0, T_PLUS, T_PAREN_L, T_NUM 2.0, T_PLUS, T_NUM 3.0, T_PAREN_R, T_PAREN_R]

    , makeTestCase "expr" -- 1 + ( 2 + 3 ) + 4
          (Expression
              (NumberExpr 1.0)
              [ BinOpRHS OpPlus (ParenExpr (Expression (NumberExpr 2.0) [BinOpRHS OpPlus (NumberExpr 3.0)]))
              , BinOpRHS OpPlus (NumberExpr 4.0)
              ],
          [])
          parseExpression [T_NUM 1.0, T_PLUS, T_PAREN_L, T_NUM 2.0, T_PLUS, T_NUM 3.0, T_PAREN_R, T_PLUS, T_NUM 4.0]

    , makeTestCase "expr" -- ( 1 + ( 2 + 3 ) + 4 ) + 5
          (Expression
              (ParenExpr (Expression (NumberExpr 1.0)
                  [ BinOpRHS OpPlus (ParenExpr (Expression (NumberExpr 2.0) [BinOpRHS OpPlus (NumberExpr 3.0)]))
                  , BinOpRHS OpPlus (NumberExpr 4.0)
                  ]))
              [BinOpRHS OpPlus (NumberExpr 5.0)],
          [])
          parseExpression [T_PAREN_L, T_NUM 1.0, T_PLUS, T_PAREN_L, T_NUM 2.0,
                           T_PLUS, T_NUM 3.0, T_PAREN_R, T_PLUS, T_NUM 4.0, T_PAREN_R, T_PLUS, T_NUM 5.0]

    , makeTestCase "expr" -- 1 + a
          (Expression (NumberExpr 1.0) [ BinOpRHS OpPlus (IdentExpr "a")], [])
          parseExpression [T_NUM 1.0, T_PLUS, T_IDENT "a"]

    , makeTestCase "expr" -- ( a + b + ( 2 + c ) + 4 ) + d
          (Expression
              (ParenExpr (Expression (IdentExpr "a")
                  [ BinOpRHS OpPlus (IdentExpr "b")
                  , BinOpRHS OpPlus (ParenExpr (Expression (NumberExpr 2.0) [BinOpRHS OpPlus (IdentExpr "c")]))
                  , BinOpRHS OpPlus (NumberExpr 4.0)
                  ]))
              [BinOpRHS OpPlus (IdentExpr "d")],
          [])
          parseExpression [T_PAREN_L, T_IDENT "a", T_PLUS, T_IDENT "b", T_PLUS, T_PAREN_L, T_NUM 2.0,
                           T_PLUS, T_IDENT "c", T_PAREN_R, T_PLUS, T_NUM 4.0, T_PAREN_R, T_PLUS, T_IDENT "d"]
    ]

testParseStatement = TestList
    [ makeTestCase "stmt" -- 1 ;
        (ExprStmt (Expression (NumberExpr 1.0) []), [])
        parseStatement [T_NUM 1.0, T_SEMICOLON]
    , makeTestCase "stmt" -- { 1 ; }
        (BlockStmt [ExprStmt (Expression (NumberExpr 1.0) [])], [])
        parseStatement [T_CURLY_L, T_NUM 1.0, T_SEMICOLON, T_CURLY_R]
    , makeTestCase "stmt" -- { 1 + 1 ; 2 + 2 ; }
        (BlockStmt [ ExprStmt (Expression (NumberExpr 1.0) [BinOpRHS OpPlus (NumberExpr 1.0)])
                   , ExprStmt (Expression (NumberExpr 2.0) [BinOpRHS OpPlus (NumberExpr 2.0)])
                   ],
        [])
        parseStatement [T_CURLY_L, T_NUM 1.0, T_PLUS, T_NUM 1.0, T_SEMICOLON,
                        T_NUM 2.0, T_PLUS, T_NUM 2.0, T_SEMICOLON, T_CURLY_R]
    , makeTestCase "stmt" -- { 1 + ( 2 + 3 ) ; }
        (BlockStmt [ ExprStmt (Expression (NumberExpr 1.0)
                        [BinOpRHS OpPlus (ParenExpr (Expression (NumberExpr 2.0)
                            [BinOpRHS OpPlus (NumberExpr 3.0)]))])],
        [])
        parseStatement [T_CURLY_L, T_NUM 1.0, T_PLUS, T_PAREN_L, T_NUM 2.0, T_PLUS, T_NUM 3.0,
                        T_PAREN_R, T_SEMICOLON, T_CURLY_R ]
    , makeTestCase "stmt" -- { { 1 ; } 2 ; }
        (BlockStmt [ BlockStmt [ExprStmt (Expression (NumberExpr 1.0) [])]
                   , ExprStmt (Expression (NumberExpr 2.0) [])
                   ],
        [])
        parseStatement [T_CURLY_L, T_CURLY_L, T_NUM 1.0, T_SEMICOLON, T_CURLY_R, T_NUM 2.0, T_SEMICOLON, T_CURLY_R]
    , makeTestCase "stmt" -- val a = 1, b = 2;
        (DeclStmt (DeclList DeclVal
            [ Declaration "a" (Expression (NumberExpr 1.0) [])
            , Declaration "b" (Expression (NumberExpr 2.0) [])
            ]),
        [])
        parseStatement [T_VAL, T_IDENT "a", T_ASSIGN, T_NUM 1.0, T_COMMA, T_IDENT "b", T_ASSIGN, T_NUM 2.0, T_SEMICOLON]
    ]

testParseDecls = TestList
    [ makeTestCase "decl" -- val a = 1
        (DeclList DeclVal [Declaration "a" (Expression (NumberExpr 1.0) [])], [])
        parseDecllist [T_VAL, T_IDENT "a", T_ASSIGN, T_NUM 1.0]
    , makeTestCase "decl" -- val a = 1, b = 2
        (DeclList DeclVal
            [ Declaration "a" (Expression (NumberExpr 1.0) [])
            , Declaration "b" (Expression (NumberExpr 2.0) [])
            ],
        [])
        parseDecllist [T_VAL, T_IDENT "a", T_ASSIGN, T_NUM 1.0, T_COMMA, T_IDENT "b", T_ASSIGN, T_NUM 2.0]
    , makeTestCase "decl" -- val a = (1 + 2)
        (DeclList DeclVal
            [ Declaration "a" (Expression (ParenExpr
                (Expression (NumberExpr 1.0) [BinOpRHS OpPlus (NumberExpr 2.0)])) [])
            ],
        [])
        parseDecllist [T_VAL, T_IDENT "a", T_ASSIGN, T_PAREN_L, T_NUM 1.0, T_PLUS, T_NUM 2.0, T_PAREN_R]
    ]


makeTestCase label expected f args = TestCase (assertEqual (label ++ ": " ++ (unwords $ map show args))
                                               expected (f args))
