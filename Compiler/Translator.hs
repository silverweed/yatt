module Compiler.Translator where

import Compiler.Parser
import Test.HUnit
import Data.List

ind = "\t"
endl = "\n"

prelude = unwords
    [ "int main() {"
    , endl
    ]

postlude = unwords
    [ "}"
    , endl
    ]

class ASTNode a where
    translate :: a -> String

instance ASTNode Program where
    translate (Program stmt) = prelude ++ ind ++ (translate stmt) ++ endl ++ postlude

instance ASTNode Statement where
    translate (BlockStmt stmts)   = "{" ++ (intercalate endl $ map translate stmts) ++ "}"
    translate (ExprStmt expr)     = translate expr ++ ";"
    translate (DeclStmt decllist) = translate decllist ++ ";"

instance ASTNode Expression where
    translate (Expression primary binoprhs) = translate primary ++ (unwords $ map translate binoprhs)

instance ASTNode BinOpRHS where
    translate (BinOpRHS binop primary) = translate binop ++ translate primary

instance ASTNode PrimaryExpr where
    translate (IdentExpr name) = name
    translate (NumberExpr n)   = show n
    translate (ParenExpr expr) = "(" ++ translate expr ++ ")"

instance ASTNode BinOp where
    translate OpPlus = "+"

instance ASTNode DeclList where
    translate (DeclList DeclVal decls) = "const double " ++ (intercalate ", " $ map translate decls)

instance ASTNode Declaration where
    translate (Declaration id expr) = id ++ " = " ++ translate expr
