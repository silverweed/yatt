module Compiler.Lexer where

import Prelude hiding (lex)
import Text.Read (readMaybe)
import Test.HUnit
import Data.Char
import Data.List

data Token = T_PLUS
           | T_MINUS
           | T_MUL
           | T_DIV
           | T_PLUSEQUAL
           | T_PLUSPLUS
           | T_MINUSEQUAL
           | T_MINUSMINUS
           | T_MULEQUAL
           | T_DIVEQUAL
           | T_MULMUL
           | T_DIVDIV
           | T_CURLY_L
           | T_CURLY_R
           | T_SEMICOLON
           | T_PAREN_L
           | T_PAREN_R
           | T_VAL
           | T_VAR
           | T_EQUAL
           | T_EQUALEQUAL
           | T_COMMA
           | T_INCL
           | T_NUM     Double
           | T_IDENT   String
           | T_STRING  String
           | T_INVALID String
           deriving (Eq)

instance Show Token where
    show T_PLUS          = "+"
    show T_MINUS         = "-"
    show T_MUL           = "*"
    show T_DIV           = "/"
    show T_PLUSEQUAL     = "+="
    show T_PLUSPLUS      = "++"
    show T_MINUSEQUAL    = "-="
    show T_MINUSMINUS    = "--"
    show T_MULEQUAL      = "*="
    show T_MULMUL        = "**"
    show T_DIVDIV        = "//"
    show T_DIVEQUAL      = "/="
    show T_CURLY_L       = "{"
    show T_CURLY_R       = "}"
    show T_SEMICOLON     = ";"
    show T_PAREN_L       = "("
    show T_PAREN_R       = ")"
    show T_COMMA         = ","
    show T_EQUAL         = "="
    show T_EQUALEQUAL    = "=="
    show T_VAL           = "val" -- declare a read-only variable
    show T_VAR           = "var"
    show T_INCL          = "inl" -- include a C header
    show (T_IDENT name)  = name
    show (T_NUM n)       = show n
    show (T_STRING s)    = '"' : s ++ "\""
    show (T_INVALID msg) = "Invalid token: " ++ msg

lex :: String -> [Token]
lex "" = []
lex input = let (tok, rest) = getToken input in tok : lex rest
                {-case tok of-}
                {-T_INVALID _ -> error $ show tok-}
                {-_           -> tok : lex rest-}

getToken :: String -> (Token, String)
getToken s@(ch:chs) | isSpace ch = getToken chs
                    | isDigit ch = matchNum s
                    | ch == '+'  = case compare (length chs) 0 of
                                       GT -> case head chs of
                                                 '=' -> (T_PLUSEQUAL, tail chs)
                                                 '+' -> (T_PLUSPLUS, tail chs)
                                                 _   -> (T_PLUS, chs)
                                       EQ -> (T_PLUS, chs)
                    | ch == '-'  = case compare (length chs) 0 of
                                       GT -> case head chs of
                                                 '=' -> (T_MINUSEQUAL, tail chs)
                                                 '-' -> (T_MINUSMINUS, tail chs)
                                                 _   -> (T_MINUS, chs)
                                       EQ -> (T_MINUS, chs)
                    | ch == '*'  = case compare (length chs) 0 of
                                       GT -> case head chs of
                                                 '=' -> (T_MULEQUAL, tail chs)
                                                 '*' -> (T_MULMUL, tail chs)
                                                 _   -> (T_MUL, chs)
                                       EQ -> (T_MUL, chs)
                    | ch == '/'  = case compare (length chs) 0 of
                                       GT -> case head chs of
                                                 '=' -> (T_DIVEQUAL, tail chs)
                                                 '/' -> (T_DIVDIV, tail chs)
                                                 _   -> (T_DIV, chs)
                                       EQ -> (T_DIV, chs)
                    | ch == '{'  = (T_CURLY_L, chs)
                    | ch == '}'  = (T_CURLY_R, chs)
                    | ch == ';'  = (T_SEMICOLON, chs)
                    | ch == '('  = (T_PAREN_L, chs)
                    | ch == ')'  = (T_PAREN_R, chs)
                    | ch == '='  = case compare (length chs) 0 of
                                       GT -> case head (chs) of
                                                 '=' -> (T_EQUALEQUAL, tail chs)
                                                 _   -> (T_EQUAL, chs)
                                       EQ -> (T_EQUAL, chs)
                    | ch == ','  = (T_COMMA, chs)
                    | ch == '"'  = matchString s
                    | isAlpha ch = matchIdent s
                    | otherwise  = (T_INVALID s, "")

matchNum :: String -> (Token, String)
matchNum s = let (lexeme, rest) = span isNumComponent s in
                 case readMaybe lexeme :: Maybe Double of
                     Just d  -> (T_NUM d, rest)
                     Nothing -> (T_INVALID lexeme, rest)
             where
             isNumComponent x = isDigit x || x `elem` ".eE"

matchIdent :: String -> (Token, String)
matchIdent s | lexeme == "val"  = (T_VAL, rest)
             | lexeme == "var"  = (T_VAR, rest)
             | lexeme == "inl"  = (T_INCL, rest)
             | otherwise        = (T_IDENT lexeme, rest)
             where
             (lexeme, rest)     = span isIdentComponent s
             isIdentComponent s = isAlphaNum s || s `elem` "_$"

-- parses a string. Accepts multiline strings.
matchString :: String -> (Token, String)
matchString s@(ch:chs) | ch == '"' = let (strcontent, rest) = break (== '"') chs in
                                         case length rest > 0 && head rest == '"' of
                                             True  -> (T_STRING strcontent, tail rest)
                                             False -> (T_INVALID $ ch:strcontent, rest)
                       | otherwise = error $ "Couldn't match string: " ++ s


invalidToken :: String -> a
invalidToken tok = error $ "Invalid token: " ++ tok

--------------------------------------- TESTS

testLexPlus = TestList
              [ makeTestCase lex "+"   [T_PLUS]
              , makeTestCase lex "++"  [T_PLUSPLUS]
              , makeTestCase lex "+ +" [T_PLUS, T_PLUS]
              , makeTestCase lex "+="  [T_PLUSEQUAL]
              , makeTestCase lex "+++" [T_PLUSPLUS, T_PLUS]
              ]
testLexMinus = TestList
              [ makeTestCase lex "-"   [T_MINUS]
              , makeTestCase lex "--"  [T_MINUSMINUS]
              , makeTestCase lex "- -" [T_MINUS, T_MINUS]
              , makeTestCase lex "-="  [T_MINUSEQUAL]
              , makeTestCase lex "---" [T_MINUSMINUS, T_MINUS]
              ]
testLexMul = TestList
              [ makeTestCase lex "*"   [T_MUL]
              , makeTestCase lex "**"  [T_MULMUL]
              , makeTestCase lex "* *" [T_MUL, T_MUL]
              , makeTestCase lex "*="  [T_MULEQUAL]
              , makeTestCase lex "***" [T_MULMUL, T_MUL]
              ]
testLexDiv = TestList
              [ makeTestCase lex "/"   [T_DIV]
              , makeTestCase lex "//"  [T_DIVDIV]
              , makeTestCase lex "/ /" [T_DIV, T_DIV]
              , makeTestCase lex "/="  [T_DIVEQUAL]
              , makeTestCase lex "///" [T_DIVDIV, T_DIV]
              ]
testLexEqual = TestList
               [ makeTestCase lex "="   [T_EQUAL]
               , makeTestCase lex "=="  [T_EQUALEQUAL]
               , makeTestCase lex "= =" [T_EQUAL, T_EQUAL]
               , makeTestCase lex "===" [T_EQUALEQUAL, T_EQUAL]
               ]
testLexComma = makeTestCase lex "," [T_COMMA]
testLexNum = TestList
             [  makeTestCase lex "1"      [T_NUM 1.0]
             ,  makeTestCase lex "10"     [T_NUM 10.0]
             ,  makeTestCase lex "1.0"    [T_NUM 1.0]
             ,  makeTestCase lex "1e2"    [T_NUM 1e2]
             ,  makeTestCase lex "1."     [T_INVALID "1."]
             ,  makeTestCase lex "1.0.1"  [T_INVALID "1.0.1"]
             ,  makeTestCase lex "1e10e2" [T_INVALID "1e10e2"]
             ]
testLexCurly = TestList
               [ makeTestCase lex  "{"         [T_CURLY_L]
               , makeTestCase lex  "}"         [T_CURLY_R]
               , makeTestCase lex  "{ 1 + 2 }" [T_CURLY_L, T_NUM 1.0, T_PLUS, T_NUM 2.0, T_CURLY_R]
               ]
testLexSemicolon = makeTestCase lex ";" [T_SEMICOLON]
testLexParen = TestList
               [ makeTestCase lex  "("         [T_PAREN_L]
               , makeTestCase lex  ")"         [T_PAREN_R]
               , makeTestCase lex  "( 1 + 2 )" [T_PAREN_L, T_NUM 1.0, T_PLUS, T_NUM 2.0, T_PAREN_R]
               ]
testLexGetToken = TestList
                  [ makeTestCase getToken "{"    (T_CURLY_L, "")
                  , makeTestCase getToken "{}"   (T_CURLY_L, "}")
                  , makeTestCase getToken "++"   (T_PLUSPLUS, "")
                  , makeTestCase getToken "+++"  (T_PLUSPLUS, "+")
                  , makeTestCase getToken "123+" (T_NUM 123.0, "+")
                  , makeTestCase getToken "0.2"  (T_NUM 0.2, "")
                  ]
testLexIdent = TestList
               [ makeTestCase lex "val"  [T_VAL]
               , makeTestCase lex "var"  [T_VAR]
               , makeTestCase lex "vals" [T_IDENT "vals"]
               , makeTestCase lex "varr" [T_IDENT "varr"]
               , makeTestCase lex "inl"  [T_INCL]
               ]

testLexString = TestList
                [ makeTestCase lex "\"string\""   [T_STRING "string"]
                , makeTestCase lex "\"\""         [T_STRING ""]
                , makeTestCase lex "\""           [T_INVALID "\""]
                , makeTestCase lex "\"string\"\"" [T_STRING "string", T_INVALID "\""]
                , makeTestCase lex "\"string\nwith\nnewlines\tand tabs\"" [T_STRING "string\nwith\nnewlines\tand tabs"]
                ]

testLexFunc = TestList
              [ makeTestCase lex "printf(\"%s\", a);" [T_IDENT "printf", T_PAREN_L, T_STRING "%s", T_COMMA,
                                                       T_IDENT "a", T_PAREN_R, T_SEMICOLON]
              ]

testLex = TestList
          [ makeTestCase lex "1 + 2.4"      [T_NUM 1.0, T_PLUS, T_NUM 2.4]
          , makeTestCase lex "2+3-4"        [T_NUM 2.0, T_PLUS, T_NUM 3.0, T_MINUS, T_NUM 4.0]
          , makeTestCase lex "2 * 3 / 4"    [T_NUM 2.0, T_MUL, T_NUM 3.0, T_DIV, T_NUM 4.0]
          , makeTestCase lex "val a=1;"     [T_VAL, T_IDENT "a", T_EQUAL, T_NUM 1.0, T_SEMICOLON]
          , makeTestCase lex "val a=1,b=2;" [T_VAL, T_IDENT "a", T_EQUAL, T_NUM 1.0, T_COMMA,
                                             T_IDENT "b", T_EQUAL, T_NUM 2.0, T_SEMICOLON]
          ]

makeTestCase f arg expected = TestCase (assertEqual ("lex: " ++ arg) expected (f arg))
