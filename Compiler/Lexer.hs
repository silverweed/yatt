module Compiler.Lexer where

import Prelude hiding (lex)
import Text.Read (readMaybe)
import Test.HUnit

data Token = T_PLUS
           | T_NUM Double
           | T_CURLY_L
           | T_CURLY_R
           | T_SEMICOLON
           | T_PAREN_L
           | T_PAREN_R
           | T_INVALID String
           deriving (Eq)

instance Show Token where
    show T_PLUS          = "+"
    show (T_NUM n)       = show n
    show (T_INVALID msg) = "Invalid token: " ++ msg
    show T_CURLY_L       = "{"
    show T_CURLY_R       = "}"
    show T_SEMICOLON     = ";"
    show T_PAREN_L       = "("
    show T_PAREN_R       = ")"

lex :: String -> [Token]
lex input = map matchToken $ words input

matchToken :: String -> Token
matchToken "+" = T_PLUS
matchToken "{" = T_CURLY_L
matchToken "}" = T_CURLY_R
matchToken ";" = T_SEMICOLON
matchToken "(" = T_PAREN_L
matchToken ")" = T_PAREN_R
matchToken n   = case readMaybe n :: Maybe Double of
                     Just d  -> T_NUM d
                     Nothing -> T_INVALID n


invalidToken :: String -> a
invalidToken tok = error $ "Invalid token: " ++ tok

--------------------------------------- TESTS

testLexPlus = makeTestCase "+" [T_PLUS]
testLexNum = TestList
             [  makeTestCase "1"     [T_NUM 1.0]
             ,  makeTestCase "10"    [T_NUM 10.0]
             ,  makeTestCase "1.0"   [T_NUM 1.0]
             ,  makeTestCase "1e2"   [T_NUM 1e2]
             ,  makeTestCase "1."    [T_INVALID "1."]
             ,  makeTestCase "1.0.1" [T_INVALID "1.0.1"]
             ]
testLexCurly = TestList
               [ makeTestCase "{"         [T_CURLY_L]
               , makeTestCase "}"         [T_CURLY_R]
               , makeTestCase "{ 1 + 2 }" [T_CURLY_L, T_NUM 1.0, T_PLUS, T_NUM 2.0, T_CURLY_R]
               ]
testLexSemicolon = makeTestCase ";" [T_SEMICOLON]
testLexParen = TestList
               [ makeTestCase "("         [T_PAREN_L]
               , makeTestCase ")"         [T_PAREN_R]
               , makeTestCase "( 1 + 2 )" [T_PAREN_L, T_NUM 1.0, T_PLUS, T_NUM 2.0, T_PAREN_R]
               ]
testLex = TestList
          [ makeTestCase "1 + 2.4"   [T_NUM 1.0, T_PLUS, T_NUM 2.4]
          -- FIXME
          , makeTestCase "2+3+4"     [T_NUM 2.0, T_PLUS, T_NUM 3.0, T_PLUS, T_NUM 4.0]
          , makeTestCase "2 + 3 + 4" [T_NUM 2.0, T_PLUS, T_NUM 3.0, T_PLUS, T_NUM 4.0]
          ]

makeTestCase arg expected = TestCase (assertEqual ("lex: " ++ arg) expected (lex arg))
