

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
>           --  | Fun [String] Expr
>           | Let [(String,Expr)] Expr
>           | LetRec [(String,Expr)] Expr
>           | Block [Stmt]
>           deriving (Eq,Show,Data,Typeable,Generic) 

> data Selector = Num Scientific
>               | Str String
>           deriving (Eq,Show,Data,Typeable,Generic) 

where to add blocks:
if?
ask?
let?
lam?

could make blocks implicit in this syntax?

review where explicit and implicit blocks can go

 > data Block = Block [Stmt]
 >           deriving (Eq,Show) 

> data Stmt = StExpr Expr
>           | When Expr Expr
>           | LetStmt String Expr
>           deriving (Eq,Show,Data,Typeable,Generic) 
