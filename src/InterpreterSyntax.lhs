
The syntax which the interpreter uses

> module InterpreterSyntax where

> import Data.Scientific (Scientific)

> data Expr = Num Scientific
>           | Str String
>           | Iden String
>           | If Expr Expr Expr
>           | App Expr Expr
>           | Lam String Expr
>           | AppHaskell String [Expr]
>           | Block [Expr]
>           deriving (Eq,Show) 
