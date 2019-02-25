

The high level syntax which the parser produces

> module Syntax where

> import Data.Scientific (Scientific)

> data Expr = True
>           | False
>           | Num Scientific
>           | Str String
>           | Iden String

>           | Parens Expr

>           --  | TupleCtor [Expr]
>           --  | TupleGet Expr Integer
> 
>           | If [(Expr,Expr)] (Maybe Expr)
>           | Ask [(Expr,Expr)] (Maybe Expr)
>
>           | App Expr [Expr]
>           | UnaryMinus Expr
>           | BinOp Expr String Expr
>
>           | Lam [String] Expr
>           --  | Fun [String] Expr
>           | Let [(String,Expr)] Expr
>           deriving (Eq,Show) 
