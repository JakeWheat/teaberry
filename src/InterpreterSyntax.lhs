
The syntax which the interpreter uses

> module InterpreterSyntax where

> import Data.Scientific (Scientific)

> data Expr = Sel Selector
>           | Iden String
>           | If Expr Expr Expr
>           | App Expr Expr
>           | Lam String Expr
>           | Let String Expr Expr
>           | AppHaskell String [Expr]
>           | Block [Expr]
>           deriving (Eq,Show)

> data Selector = Num Scientific
>               | Str String
>               deriving (Eq,Show)
