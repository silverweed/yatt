module Main where

import Prelude hiding (lex)
import Test.HUnit
import Compiler.Lexer
import Compiler.Parser

main :: IO ()
main = do runTestTT tests
          return ()

compile = show . parse . lex

tests = TestList [ TestList testsLexer, TestList testsParser ]

testsLexer = [ TestLabel "testLexPlus" testLexPlus
             , TestLabel "testLexNum" testLexNum
             , TestLabel "testLexCurly" testLexCurly
             , TestLabel "testLexParen" testLexParen
             , TestLabel "testLexSemicolon" testLexSemicolon
             , TestLabel "testLex" testLex
             ]
testsParser = [ TestLabel "testParseNum" testParseNum
              , TestLabel "testParseExpr" testParseExpr
              , TestLabel "testParseStatement" testParseStatement
              ]
