

The high level syntax which the parser produces

> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> module Syntax (Stmt(..)
>               ,Shadow(..)
>               ,Expr(..)
>               ,Selector(..)
>               ,VariantDecl(..)
>               ,Pat(..)
>               ,PatName(..)
>               ,Module(..)
>               ,PreludeStmt(..)
>               ,ProvideItem(..)
>               ,ImportSource(..)
>               ,Ref(..)
>               ,extractInt
>               ) where

> import Data.Data (Data)
> import Scientific (Scientific,extractInt)

------------------------------------------------------------------------------

> data Module = Module [PreludeStmt] [Stmt]
>              deriving (Eq,Show,Data)

> data PreludeStmt = Provide [ProvideItem]
>                  | Import ImportSource String
>                  | Include ImportSource
>                  | IncludeFrom String [ProvideItem]
>                  | ImportNames [String] ImportSource
>                  deriving (Eq,Show,Data)

import n1, n2, ... from <import-source>
so:
import n1 as n2 from <import-source>
  not supported?
import * from <import-source>
  not supported? OK, this is very redundant with include <import-source>


> data ProvideItem = ProvideAll
>                  | ProvideAlias String String
>                  | ProvideName String
>                  deriving (Eq,Show,Data) 

> data ImportSource = ImportSpecial String [String]
>                   | ImportName String
>                   deriving (Eq,Show,Data) 


> data Stmt = StExpr Expr
>           | When Expr Expr
>           | LetDecl Pat Expr
>           | RecDecl Pat Expr
>           | FunDecl PatName -- name
>                     [Pat] -- args
>                     Expr -- body
>                     (Maybe [Stmt]) -- test block
>           | VarDecl Pat Expr -- todo: make patname
>           | SetVar String Expr
>           | SetRef Expr [(String,Expr)]
>           | DataDecl String [VariantDecl] (Maybe [Stmt])
>           | TPred Expr String Expr Expr -- is%, is-not%
>           | TPostfixOp Expr String --- does-not-raise
>           | Check (Maybe String) [Stmt]
>           deriving (Eq,Show,Data) 

> data VariantDecl = VariantDecl String [(Ref,String)]
>                  deriving (Eq,Show,Data) 

> data Ref = Ref | Con
>          deriving (Eq,Show,Data) 

revisit patterns again?
recdecl, vardecl: patname only
letdecl: do patname only
  and do another let for tuple binding?
same with let
would this make things simpler?
cases has a different pattern
  it doesn't support tuple, but supports the rest
the problem with these sort of changes is you end up in the mess
that is parse expression or pattern
this needs a good rethink first

> data Pat = IdenP PatName
>          | VariantP (Maybe String) String [Pat]
>          | TupleP [Pat]
>          | AsP Pat PatName
>           deriving (Eq,Show,Data) 

> data PatName = PatName Shadow String
>              deriving (Eq,Show,Data) 

> data Shadow = NoShadow | Shadow
>           deriving (Eq,Show,Data) 


> data Expr = Sel Selector
>           | Iden String
>           | Parens Expr
>           | TupleGet Expr Int
>           | Construct Expr [Expr]
>           | DotExpr Expr String
>           | If [(Expr,Expr)] (Maybe Expr)
>           | Ask [(Expr,Expr)] (Maybe Expr)
>           | Cases String Expr [(Pat, Expr)] (Maybe Expr)
>           | App Expr [Expr]
>           | UnaryMinus Expr
>           | BinOp Expr String Expr
>           | Lam [Pat] Expr
>           | Let [(Pat,Expr)] Expr
>           | LetRec [(Pat,Expr)] Expr
>           | Block [Stmt]
>           | UnboxRef Expr String
>           deriving (Eq,Show,Data) 

todo: merge this into expr
have to fix the pattern or expression parser first

> data Selector = Num Scientific
>               | Text String
>               | TupleSel [Expr]
>               | RecordSel [(String,Expr)]
>               deriving (Eq,Show,Data) 

