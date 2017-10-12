module Compiler.Lexer where

import Prelude hiding (lex)
import Text.Read (readMaybe)
import Test.HUnit
import Data.Char
import Data.List

data Token = T_PLUS
           | T_PLUSEQUAL
           | T_PLUSPLUS
           | T_NUM Double
           | T_CURLY_L
           | T_CURLY_R
           | T_SEMICOLON
           | T_PAREN_L
           | T_PAREN_R
           | T_INVALID String
           | T_VAL
           | T_ASSIGN
           | T_COMMA
           | T_EQUALS
           | T_IDENT String
           deriving (Eq)

instance Show Token where
    show T_PLUS          = "+"
    show T_PLUSEQUAL     = "+="
    show T_PLUSPLUS      = "++"
    show (T_NUM n)       = show n
    show (T_INVALID msg) = "Invalid token: " ++ msg
    show T_CURLY_L       = "{"
    show T_CURLY_R       = "}"
    show T_SEMICOLON     = ";"
    show T_PAREN_L       = "("
    show T_PAREN_R       = ")"
    show T_VAL           = "val" -- declare a read-only variable
    show T_ASSIGN        = "="
    show T_COMMA         = ","
    show T_EQUALS        = "=="
    show (T_IDENT name)  = name

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
                    | ch == '{'  = (T_CURLY_L, chs)
                    | ch == '}'  = (T_CURLY_R, chs)
                    | ch == ';'  = (T_SEMICOLON, chs)
                    | ch == '('  = (T_PAREN_L, chs)
                    | ch == ')'  = (T_PAREN_R, chs)
                    | ch == '='  = case compare (length chs) 0 of
                                       GT -> case head (chs) of
                                                 '=' -> (T_EQUALS, tail chs)
                                                 _   -> (T_ASSIGN, chs)
                                       EQ -> (T_ASSIGN, chs)
                    | ch == ',' = (T_COMMA, chs)
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
matchIdent s | lexeme == show T_VAL = (T_VAL, rest)
             | otherwise            = (T_IDENT lexeme, rest)
             where
             (lexeme, rest) = span isIdentComponent s
             isIdentComponent s = isAlphaNum s || s `elem` "_$"

matchToken :: String -> Token
matchToken n   = case readMaybe n :: Maybe Double of
                     Just d  -> T_NUM d
                     Nothing -> T_INVALID n


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
testLexEqual = TestList
               [ makeTestCase lex "="   [T_ASSIGN]
               , makeTestCase lex "=="  [T_EQUALS]
               , makeTestCase lex "= =" [T_ASSIGN, T_ASSIGN]
               , makeTestCase lex "===" [T_EQUALS, T_ASSIGN]
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
          , makeTestCase lex "vals" [T_IDENT "vals"]
          ]
testLex = TestList
          [ makeTestCase lex "1 + 2.4"     [T_NUM 1.0, T_PLUS, T_NUM 2.4]
          , makeTestCase lex "2+3+4"       [T_NUM 2.0, T_PLUS, T_NUM 3.0, T_PLUS, T_NUM 4.0]
          , makeTestCase lex "2 + 3 + 4"   [T_NUM 2.0, T_PLUS, T_NUM 3.0, T_PLUS, T_NUM 4.0]
          , makeTestCase lex "val a=1;"    [T_VAL, T_IDENT "a", T_ASSIGN, T_NUM 1.0, T_SEMICOLON]
          , makeTestCase lex "val a=1,b=2;" [T_VAL, T_IDENT "a", T_ASSIGN, T_NUM 1.0, T_COMMA, T_IDENT "b", T_ASSIGN, T_NUM 2.0, T_SEMICOLON]
          ]

makeTestCase f arg expected = TestCase (assertEqual ("lex: " ++ arg) expected (f arg))
