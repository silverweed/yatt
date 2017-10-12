module Main where

import Prelude hiding (lex)
import Data.Char (isSpace)
import Test.HUnit
import Compiler.Lexer
import Compiler.Parser
import Compiler.Translator

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

main :: IO ()
{-main = do runTestTT tests; return ()-}
main = interact compile

compile = translate . parse . lex . trim

tests = TestList [ TestList testsLexer, TestList testsParser ]

testsLexer = [ TestLabel "testLexPlus" testLexPlus
             , TestLabel "testLexNum" testLexNum
             , TestLabel "testLexCurly" testLexCurly
             , TestLabel "testLexParen" testLexParen
             , TestLabel "testLexSemicolon" testLexSemicolon
             , TestLabel "testLexGetToken" testLexGetToken
             , TestLabel "testLexEqual" testLexEqual
             , TestLabel "testLexComma" testLexComma
             , TestLabel "testLexIdent" testLexIdent
             , TestLabel "testLex" testLex
             ]
testsParser = [ TestLabel "testParseNum" testParseNum
              , TestLabel "testParseExpr" testParseExpr
              , TestLabel "testParseStatement" testParseStatement
              , TestLabel "testParseDecls" testParseDecls
              ]
