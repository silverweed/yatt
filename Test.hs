module Main where

import Prelude hiding (lex)
import Data.Char (isSpace)
import System.Exit
import Test.HUnit
import Compiler.Lexer
import Compiler.Parser
import Compiler.Translator

main = runTestTT tests >>= \counts ->
          case failures counts > 0 of
              True  -> exitFailure
              False -> exitSuccess

tests = TestList [ TestList testsLexer, TestList testsParser ]

testsLexer = [ TestLabel "testLexPlus" testLexPlus
             , TestLabel "testLexMinus" testLexMinus
             , TestLabel "testLexMul" testLexMul
             , TestLabel "testLexDiv" testLexDiv
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
