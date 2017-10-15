{-| Simple recursive descent parser -}
module Compiler.Parser where

import Prelude hiding (lex)
import Compiler.Lexer hiding (makeTestCase)
import Test.HUnit

type NumberType = Double

{- program : statements
 - statement : block | decllist ';' | expression ';' | include ';'
 - include : 'inl' STRING stringlist
 - stringlist : ',' STRING | EMPTY
 - block : '{' statements '}'
 - statements : statement statements | EMPTY
 - expression : primary binoprhs
 - primary : identifierexpr | NUM | STRING | '(' expression ')'
 - identifierexpr : ID | callexpr
 - callexpr : ID '(' arglist ')'
 - arglist : expression expressions | EMPTY
 - expressions : ',' expression expressions | EMPTY
 - binoprhs : BINOP primary | EMPTY
 - binop : '+'
 - decllist : declident declaration declarations
 - declident : 'val'
 - declarations : ',' declaration
 - declaration : ID '=' expression
 -}
data Program = Program [Statement] deriving (Eq, Show)

data Statement = BlockStmt [Statement]
               | ExprStmt  Expression
               | DeclStmt  DeclList
               | InclStmt  [Expression]
               deriving (Eq, Show)

data Expression = Expression PrimaryExpr [BinOpRHS]
                deriving (Eq, Show)

data DeclList = DeclList DeclType [Declaration]
              deriving (Eq, Show)

data DeclType = DeclTypeVal
              | DeclTypeVar
              deriving Eq

data Declaration = Declaration String Expression
                 deriving (Eq, Show)

data PrimaryExpr = IdentExpr  String
                 | NumberExpr NumberType
                 | StringExpr String
                 | CallExpr   String [Expression] -- func name, args
                 | ParenExpr  Expression
                 deriving (Eq, Show)

data BinOpRHS = BinOpRHS BinOp PrimaryExpr deriving (Eq, Show)

data BinOp = OpPlus
           | OpIncr
           | OpPlusEq
           | OpMinus
           | OpDecr
           | OpMinusEq
           | OpMul
           | OpExp
           | OpMulEq
           | OpDiv
           | OpIntDiv
           | OpDivEq
           | OpTestEquals
           | OpAssign
           deriving Eq

instance Show DeclType where
    show DeclTypeVal = "DeclTypeVal"
    show DeclTypeVar = "DeclTypeVar"

instance Show BinOp where
    show OpPlus       = "OpPlus"
    show OpIncr       = "OpIncr"
    show OpPlusEq     = "OpPlusEq"
    show OpMinus      = "OpMinus"
    show OpDecr       = "OpDecr"
    show OpMinusEq    = "OpMinusEq"
    show OpMul        = "OpMul"
    show OpExp        = "OpExp"
    show OpMulEq      = "OpMulEq"
    show OpDiv        = "OpDiv"
    show OpIntDiv     = "OpIntDiv"
    show OpDivEq      = "OpDivEq"
    show OpTestEquals = "OpTestEquals"
    show OpAssign     = "OpAssign"

parse :: [Token] -> Program
parse = parseProgram

parseProgram :: [Token] -> Program
parseProgram tokens = let (stmts, rest) = parseStatements tokens in
                          case rest of
                          [] -> Program stmts
                          _  -> error $ "Syntax error: extra tokens " ++ (show rest) ++ " at the end of the program"

-- parses a single statement and returns (parsed statement, tokens left)
parseStatement :: [Token] -> (Statement, [Token])
parseStatement tokens@(t:toks)
    | t == T_SEMICOLON = parseStatement toks -- ignore stray semicolons
    | t == T_CURLY_L   = parseBlock tokens
    | t == T_INCL      = checkSemicolon id       $ parseInclude toks
    | isDeclIdent t    = checkSemicolon DeclStmt $ parseDecllist tokens
    | otherwise        = checkSemicolon ExprStmt $ parseExpression tokens
    where
    checkSemicolon f expr = let (a, rest) = expr in
                                case length rest > 0 && head rest == T_SEMICOLON of
                                    True  -> (f a, tail rest)
                                    False -> (f a, rest) -- Loose behaviour: semicolons not required
                                             --error $ "Missing ';' at the end of statement! (have " ++ (show rest) ++ ")"

isDeclIdent :: Token -> Bool
isDeclIdent T_VAL = True
isDeclIdent T_VAR = True
isDeclIdent _     = False

parseBlock :: [Token] -> (Statement, [Token])
parseBlock (t:toks)
    | t == T_CURLY_L = let (stmts, r:rest) = parseStatements toks in
                           case r of
                               T_CURLY_R -> (BlockStmt stmts, rest)
                               _ -> error $ "Missing '}' at the end of block! (have " ++ (show r) ++ ")"
    | otherwise = error $ "Expected a block, but got " ++ (show t)

parseStatements :: [Token] -> ([Statement], [Token])
parseStatements [] = ([], [])
parseStatements tokens@(t:toks) | t == T_CURLY_R = ([], tokens)
                                | otherwise      = (stmt:stmts, rests)
                                                   where
                                                   (stmt, rest)   = parseStatement tokens
                                                   (stmts, rests) = parseStatements rest

parseDecllist :: [Token] -> (DeclList, [Token])
parseDecllist (t:toks) | t == T_VAL = (DeclList DeclTypeVal $ decl:decls, rest2)
                       | t == T_VAR = (DeclList DeclTypeVar $ decl:decls, rest2)
                       | otherwise  = error $ "Unknown declident " ++ (show t)
                       where
                       (decl, rest)   = parseDeclaration toks
                       (decls, rest2) = parseDeclarations rest

parseDeclaration :: [Token] -> (Declaration, [Token])
parseDeclaration ((T_IDENT id):T_EQUAL:toks) = let (expr, rest) = parseExpression toks in
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
parsePrimary (T_PAREN_L:toks)           = parseParenExpr toks
parsePrimary ((T_NUM n):toks)           = (NumberExpr n, toks)
parsePrimary ((T_STRING s):toks)        = (StringExpr s, toks)
parsePrimary tokens@((T_IDENT id):toks) = parseIdentExpr tokens
parsePrimary (other:toks)               = error $ "Expected primary expression, not '" ++ (show other) ++ "'"


parseIdentExpr :: [Token] -> (PrimaryExpr, [Token])
parseIdentExpr tokens@((T_IDENT id):T_PAREN_L:toks) = parseCallExpr tokens
parseIdentExpr ((T_IDENT id):toks)                  = (IdentExpr id, toks)
parseIdentExpr (other:toks) = error $ "Expected identifier or function call, not '" ++ (show other) ++ "'"

-- parses a function call: funcname(arglist)
parseCallExpr :: [Token] -> (PrimaryExpr, [Token])
parseCallExpr ((T_IDENT id):T_PAREN_L:toks) = let (args, rest) = parseArgList toks in
                                                  case length rest > 0 && head rest == T_PAREN_R of
                                                      True  -> (CallExpr id args, tail rest)
                                                      False -> error "Missing ')' at the end of call expression!"

-- parses a list of optional comma-separated expressions until ')' is found
parseArgList :: [Token] -> ([Expression], [Token])
parseArgList tokens@(t:toks)
    | t == T_PAREN_R = ([], tokens)
    | otherwise      = let (exprs, rest) = parseExprList tokens in
                           case length rest > 0 && head rest == T_PAREN_R of
                               True  -> (exprs, rest)
                               False -> error "Exprected ')' after arglist!"
parseArglist other = error $ "Expected arglist but got " ++ (show other) ++ "!"

-- parses a (non-optional) comma-separated list of expression
parseExprList :: [Token] -> ([Expression], [Token])
parseExprList tokens = let (expr, rest) = parseExpression tokens in
                           case length rest > 0 && head rest == T_COMMA of
                               True  -> (expr:exprs, rest2)
                                        where
                                        (exprs, rest2) = parseExprList $ tail rest
                               False -> ([expr], rest)

parseInclude :: [Token] -> (Statement, [Token])
parseInclude toks = case all isStringExpr incs of
                        True  -> (InclStmt incs, rest)
                        False -> error "Arguments of 'inl' must be string expressions!"
                    where
                    (incs, rest) = parseExprList toks
                    isStringExpr :: Expression -> Bool
                    isStringExpr (Expression (StringExpr _) []) = True
                    isStringExpr _                              = False

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
parseBinOp T_PLUS       = Just OpPlus
parseBinOp T_PLUSPLUS   = Just OpIncr
{-parseBinOp T_PLUSEQUAL  = Just OpPlusEq-}
parseBinOp T_MINUS      = Just OpMinus
parseBinOp T_MINUSMINUS = Just OpDecr
{-parseBinOp T_MINUSEQUAL = Just OpMinusEq-}
parseBinOp T_MUL        = Just OpMul
parseBinOp T_MULMUL     = Just OpExp
{-parseBinOp T_MULEQUAL   = Just OpMulEq-}
parseBinOp T_DIV        = Just OpDiv
parseBinOp T_DIVDIV     = Just OpIntDiv
{-parseBinOp T_DIVEQUAL   = Just OpDivEq-}
parseBinOp T_EQUALEQUAL = Just OpTestEquals
parseBinOp T_EQUAL      = Just OpAssign
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

    , makeTestCase "expr" -- 1 + 2 - 3 * 4 / 5
          (Expression (NumberExpr 1.0)
              [ BinOpRHS OpPlus  (NumberExpr 2.0)
              , BinOpRHS OpMinus (NumberExpr 3.0)
              , BinOpRHS OpMul   (NumberExpr 4.0)
              , BinOpRHS OpDiv   (NumberExpr 5.0)
              ], [])
          parseExpression [T_NUM 1.0, T_PLUS, T_NUM 2.0, T_MINUS, T_NUM 3.0, T_MUL, T_NUM 4.0, T_DIV, T_NUM 5.0]

    , makeTestCase "expr" -- ( 1 + 2 ) // 3
          (Expression
              (ParenExpr (Expression (NumberExpr 1.0) [BinOpRHS OpPlus (NumberExpr 2.0)]))
              [BinOpRHS OpIntDiv (NumberExpr 3.0)],
          [])
          parseExpression [T_PAREN_L, T_NUM 1.0, T_PLUS, T_NUM 2.0, T_PAREN_R, T_DIVDIV, T_NUM 3.0]

    , makeTestCase "expr" -- ( 1 ** ( 2 + 3 ) )
          (Expression
              (ParenExpr (Expression (NumberExpr 1.0)
                  [ BinOpRHS OpExp (ParenExpr (Expression (NumberExpr 2.0) [BinOpRHS OpPlus (NumberExpr 3.0)]))
                  ]))
              [],
          [])
          parseExpression [T_PAREN_L, T_NUM 1.0, T_MULMUL, T_PAREN_L, T_NUM 2.0, T_PLUS,
                           T_NUM 3.0, T_PAREN_R, T_PAREN_R]

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

    , makeTestCase "expr" -- 1 == a
          (Expression (NumberExpr 1.0) [ BinOpRHS OpTestEquals (IdentExpr "a")], [])
          parseExpression [T_NUM 1.0, T_EQUALEQUAL, T_IDENT "a"]

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
        (DeclStmt (DeclList DeclTypeVal
            [ Declaration "a" (Expression (NumberExpr 1.0) [])
            , Declaration "b" (Expression (NumberExpr 2.0) [])
            ]),
        [])
        parseStatement [T_VAL, T_IDENT "a", T_EQUAL, T_NUM 1.0, T_COMMA, T_IDENT "b", T_EQUAL, T_NUM 2.0, T_SEMICOLON]

    , makeTestCase "stmt" -- { val a = 1; var b = 2; }
        (BlockStmt [ DeclStmt (DeclList DeclTypeVal [Declaration "a" (Expression (NumberExpr 1.0) [])])
                   , DeclStmt (DeclList DeclTypeVar [Declaration "b" (Expression (NumberExpr 2.0) [])])
                   ],
        [])
        parseStatement [T_CURLY_L, T_VAL, T_IDENT "a", T_EQUAL, T_NUM 1.0, T_SEMICOLON, T_VAR, T_IDENT "b",
                        T_EQUAL, T_NUM 2.0, T_SEMICOLON, T_CURLY_R]
    ]

testParseDecls = TestList
    [ makeTestCase "decl" -- val a = 1
        (DeclList DeclTypeVal [Declaration "a" (Expression (NumberExpr 1.0) [])], [])
        parseDecllist [T_VAL, T_IDENT "a", T_EQUAL, T_NUM 1.0]

    , makeTestCase "decl" -- val a = 1, b = 2
        (DeclList DeclTypeVal
            [ Declaration "a" (Expression (NumberExpr 1.0) [])
            , Declaration "b" (Expression (NumberExpr 2.0) [])
            ],
        [])
        parseDecllist [T_VAL, T_IDENT "a", T_EQUAL, T_NUM 1.0, T_COMMA, T_IDENT "b", T_EQUAL, T_NUM 2.0]

    , makeTestCase "decl" -- val a = (1 + 2)
        (DeclList DeclTypeVal
            [ Declaration "a" (Expression (ParenExpr
                (Expression (NumberExpr 1.0) [BinOpRHS OpPlus (NumberExpr 2.0)])) [])
            ],
        [])
        parseDecllist [T_VAL, T_IDENT "a", T_EQUAL, T_PAREN_L, T_NUM 1.0, T_PLUS, T_NUM 2.0, T_PAREN_R]

    , makeTestCase "decl" -- val a = "a"
        (DeclList DeclTypeVal [Declaration "a" (Expression (StringExpr "a") [])], [])
        parseDecllist [T_VAL, T_IDENT "a", T_EQUAL, T_STRING "a"]

    , makeTestCase "decl" -- val a = "a" + 1
        (DeclList DeclTypeVal [Declaration "a" (Expression (StringExpr "a") [BinOpRHS OpPlus (NumberExpr 1.0)])], [])
        parseDecllist [T_VAL, T_IDENT "a", T_EQUAL, T_STRING "a", T_PLUS, T_NUM 1.0]

    , makeTestCase "decl" -- val a = "a" + "b"
        (DeclList DeclTypeVal [Declaration "a" (Expression (StringExpr "a") [BinOpRHS OpPlus (StringExpr "b")])], [])
        parseDecllist [T_VAL, T_IDENT "a", T_EQUAL, T_STRING "a", T_PLUS, T_STRING "b"]

    , makeTestCase "decl" -- var a = 1, b = 2
        (DeclList DeclTypeVar
            [ Declaration "a" (Expression (NumberExpr 1.0) [])
            , Declaration "b" (Expression (NumberExpr 2.0) [])
            ],
        [])
        parseDecllist [T_VAR, T_IDENT "a", T_EQUAL, T_NUM 1.0, T_COMMA, T_IDENT "b", T_EQUAL, T_NUM 2.0]
    ]

testParseCallExpr = TestList
    [ makeTestCase "call" -- funcname()
        (CallExpr "funcname" [], [])
        parseCallExpr [T_IDENT "funcname", T_PAREN_L, T_PAREN_R]

    , makeTestCase "call" -- funcname(a, b + c)
        (CallExpr "funcname" [ Expression (IdentExpr "a") []
                             , Expression (IdentExpr "b") [BinOpRHS OpPlus (IdentExpr "c")]
                             ], [])
        parseCallExpr [T_IDENT "funcname", T_PAREN_L, T_IDENT "a", T_COMMA, T_IDENT "b",
                       T_PLUS, T_IDENT "c", T_PAREN_R]

    , makeTestCase "call" -- funcname((2))
        (CallExpr "funcname" [Expression (ParenExpr (Expression (NumberExpr 2.0) [])) []], [])
        parseCallExpr [T_IDENT "funcname", T_PAREN_L, T_PAREN_L, T_NUM 2.0, T_PAREN_R, T_PAREN_R]

    , makeTestCase "call" -- funcname("hello")
        (CallExpr "funcname" [Expression (StringExpr "hello") []], [])
        parseCallExpr [T_IDENT "funcname", T_PAREN_L, T_STRING "hello", T_PAREN_R]
    ]

testParseProgram = TestList
    [ makeTestCase "program" -- val a = 1, b = a; b;
        (Program [ DeclStmt (DeclList DeclTypeVal
                     [ Declaration "a" (Expression (NumberExpr 1.0) [])
                     , Declaration "b" (Expression (IdentExpr "a") [])
                     ])
                 , ExprStmt (Expression (IdentExpr "b") [])
                 ])
        parseProgram [T_VAL, T_IDENT "a", T_EQUAL, T_NUM 1.0, T_COMMA, T_IDENT "b", T_EQUAL, T_IDENT "a",
                      T_SEMICOLON, T_IDENT "b", T_SEMICOLON]
    ]

makeTestCase label expected f args = TestCase (assertEqual (label ++ ": " ++ (unwords $ map show args))
                                               expected (f args))
