module Main where

import Prelude hiding (lex)
import Data.Char (isSpace)
import Compiler.Lexer
import Compiler.Parser
import Compiler.Translator

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

main :: IO ()
main = interact compile

compile = translate . parse . lex . trim
