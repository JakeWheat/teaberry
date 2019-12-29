

The high level syntax which the parser produces

> {-# LANGUAGE DeriveDataTypeable,DeriveGeneric #-}
> module Syntax (Stmt(..)
>               ,Expr(..)
>               ,Selector(..)
>               ,VariantDecl(..)
>               ,Pat(..)
>               )where

> import Data.Data (Data,Typeable)
> import Data.Scientific (Scientific)
> import GHC.Generics (Generic)

> data Expr = Sel Selector

>           | Iden String
>           | Parens Expr

>           | TupleGet Expr Int
>           | Construct Expr [Expr]
>           | DotExpr Expr String
> 
>           | If [(Expr,Expr)] (Maybe Expr)
>           | Ask [(Expr,Expr)] (Maybe Expr)
>           | Cases String Expr [(Pat, Expr)] (Maybe Expr)
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

> data Pat = IdenP String
>          | CtorP String [Pat]
>          | TupleP [Pat]
>           deriving (Eq,Show,Data,Typeable,Generic) 

> data Selector = Num Scientific
>               | Str String
>               | Tuple [Expr]
>           deriving (Eq,Show,Data,Typeable,Generic) 

> data Stmt = StExpr Expr
>           | When Expr Expr
>           | LetDecl String Expr
>           | RecDecl String Expr
>           | FunDecl String [String] Expr
>           | VarDecl String Expr
>           | SetVar String Expr
>           | DataDecl String [VariantDecl]
>           deriving (Eq,Show,Data,Typeable,Generic) 

> data VariantDecl = VariantDecl String [String]
>           deriving (Eq,Show,Data,Typeable,Generic) 
