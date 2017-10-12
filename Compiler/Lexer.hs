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

lex :: String -> [Token]
lex "" = []
lex input = let (tok, rest) = getToken input in tok : lex rest

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
matchIdent s = (T_INVALID s, "") -- TODO

matchToken :: String -> Token
matchToken n   = case readMaybe n :: Maybe Double of
                     Just d  -> T_NUM d
                     Nothing -> T_INVALID n


invalidToken :: String -> a
invalidToken tok = error $ "Invalid token: " ++ tok

--------------------------------------- TESTS

testLexPlus = TestList
              [ makeTestCase "+"   [T_PLUS]
              , makeTestCase "++"  [T_PLUSPLUS]
              , makeTestCase "+ +" [T_PLUS, T_PLUS]
              , makeTestCase "+="  [T_PLUSEQUAL]
              , makeTestCase "+++" [T_PLUSPLUS, T_PLUS]
              ]
testLexNum = TestList
             [  makeTestCase "1"      [T_NUM 1.0]
             ,  makeTestCase "10"     [T_NUM 10.0]
             ,  makeTestCase "1.0"    [T_NUM 1.0]
             ,  makeTestCase "1e2"    [T_NUM 1e2]
             ,  makeTestCase "1."     [T_INVALID "1."]
             ,  makeTestCase "1.0.1"  [T_INVALID "1.0.1"]
             ,  makeTestCase "1e10e2" [T_INVALID "1e10e2"]
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
