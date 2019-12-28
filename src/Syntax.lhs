

The high level syntax which the parser produces

> {-# LANGUAGE DeriveDataTypeable,DeriveGeneric #-}
> module Syntax where

> import Data.Scientific (Scientific)
> import Data.Data (Data,Typeable)
> import GHC.Generics (Generic)

> data Expr = Sel Selector

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
>           | Let [(String,Expr)] Expr
>           | LetRec [(String,Expr)] Expr
>           | Block [Stmt]
>           deriving (Eq,Show,Data,Typeable,Generic) 

> data Selector = Num Scientific
>               | Str String
>           deriving (Eq,Show,Data,Typeable,Generic) 

> data Stmt = StExpr Expr
>           | When Expr Expr
>           | LetDecl String Expr
>           | RecDecl String Expr
>           | FunDecl String [String] Expr
>           | VarDecl String Expr
>           | SetVar String Expr
>           deriving (Eq,Show,Data,Typeable,Generic) 
