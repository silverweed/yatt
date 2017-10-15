module Compiler.Translator where

import Compiler.Parser
import Test.HUnit
import Data.Bifunctor (bimap)
import Control.Monad (join)
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
    translate (Program stmts) = includes ++ endl ++ prelude ++ main ++ postlude
                                where
                                -- separate include statements from others in order to put them before main().
                                (includes, main) = join bimap (intercalate endl . map translate) $ hoistIncludes stmts
                                hoistIncludes = partition isInclStmt
                                isInclStmt (InclStmt _) = True
                                isInclStmt _            = False

instance ASTNode Statement where
    translate (BlockStmt stmts)   = "{" ++ (intercalate endl $ map translate stmts) ++ "}"
    translate (ExprStmt expr)     = translate expr ++ ";"
    translate (DeclStmt decllist) = translate decllist ++ ";"
    translate (InclStmt incs)     = intercalate endl $ map ((\inc -> "#include " ++ inc) . getstr) incs
                                    where
                                    getstr :: Expression -> String
                                    getstr (Expression (StringExpr s) _) = '"' : s ++ "\""
                                    getstr e = error $ "Tried to get string from non-string expression: " ++ (translate e)

instance ASTNode Expression where
    translate (Expression primary binoprhs) = translate primary ++ (unwords $ map translate binoprhs)

instance ASTNode BinOpRHS where
    translate (BinOpRHS binop primary) = translate binop ++ translate primary

instance ASTNode PrimaryExpr where
    translate (IdentExpr name) = name
    translate (NumberExpr n)   = show n
    translate (StringExpr s)   = '"' : s ++ "\""
    translate (ParenExpr expr) = "(" ++ translate expr ++ ")"
    translate (CallExpr fname args) = fname ++ "(" ++ (intercalate ", " $ map translate args) ++ ")"

instance ASTNode BinOp where
    translate OpPlus       = "+"
    translate OpIncr       = "++"
    translate OpPlusEq     = "+="
    translate OpMinus      = "-"
    translate OpDecr       = "--"
    translate OpMinusEq    = "-="
    translate OpMul        = "*"
    translate OpExp        = "**"
    translate OpMulEq      = "*="
    translate OpDiv        = "/"
    translate OpIntDiv     = "//"
    translate OpDivEq      = "/="
    translate OpTestEquals = "=="
    translate OpAssign     = "="

instance ASTNode DeclList where
    translate (DeclList DeclTypeVal decls) = intercalate (";" ++ endl) $ map (\decl ->
                                                 "const auto " ++ translate decl) decls
    translate (DeclList DeclTypeVar decls) = intercalate (";" ++ endl) $ map (\decl ->
                                                 "auto " ++ translate decl) decls

instance ASTNode Declaration where
    translate (Declaration id expr) = id ++ " = " ++ translate expr
