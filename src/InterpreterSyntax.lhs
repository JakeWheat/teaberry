
The syntax which the interpreter uses

> module InterpreterSyntax where

> import Data.Scientific (Scientific)

> data Expr = True
>           | False
>           | Num Scientific
>           | Str String
>           | Iden String
>           | If Expr Expr Expr
>           | App Expr Expr
>           | Lam String Expr
>           | AppHaskell String [Expr]
>           deriving (Eq,Show) 
