

The high level syntax which the parser produces

> {-# LANGUAGE DeriveDataTypeable,DeriveGeneric, ScopedTypeVariables #-}
> module Syntax (Stmt(..)
>               ,Shadow(..)
>               ,Binding(..)
>               ,Expr(..)
>               ,Selector(..)
>               ,VariantDecl(..)
>               ,Pat(..)
>               ,Program(..)
>               ,Provide(..)
>               ,ProvideTypes(..)
>               ,Import(..)
>               ,ImportSource(..)
>               ,extractInt
>               ) where

> import Data.Data (Data,Typeable)
> import Data.Scientific (Scientific,floatingOrInteger)
> import GHC.Generics (Generic)

> data Program = Program (Maybe Provide) (Maybe ProvideTypes)
>                [Import]
>                [Stmt]
>               deriving (Eq,Show)

> data Provide = ProvideAll
>              | Provide [(String, String)]
>              deriving (Eq,Show)
> data ProvideTypes = ProvideTypesAll
>                   | ProvideTypes [(String,String)]
>                   deriving (Eq,Show)

> data Import = Import ImportSource String
>             | ImportFrom [String] ImportSource
>                   deriving (Eq,Show)

> data ImportSource = ImportSpecial String [String]
>                   | ImportName String
>                   | ImportString String
>                   deriving (Eq,Show)

 

> data Stmt = StExpr Expr
>           | When Expr Expr
>           | LetDecl Binding
>           | RecDecl Binding
>           | FunDecl String [String] Expr (Maybe [Stmt])
>           | VarDecl Binding
>           | SetVar String Expr
>           | DataDecl String [VariantDecl] (Maybe [Stmt])
>           | TPred Expr String Expr Expr -- is%, is-not%
>           | TPostfixOp Expr String --- does-not-raise
>           | Check (Maybe String) [Stmt]
>           deriving (Eq,Show,Data,Typeable,Generic) 

> data Binding = Binding Pat Expr
>           deriving (Eq,Show,Data,Typeable,Generic) 

> data Pat = IdenP Shadow String
>          | CtorP String [Pat]
>          | TupleP [Pat]
>          | AsP Pat String
>           deriving (Eq,Show,Data,Typeable,Generic) 

> data Shadow = NoShadow | Shadow
>           deriving (Eq,Show,Data,Typeable,Generic) 


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
>           | Lam [String] Expr
>           | Let [Binding] Expr
>           | LetRec [Binding] Expr
>           | Block [Stmt]
>           deriving (Eq,Show,Data,Typeable,Generic) 


> data Selector = Num Scientific
>               | Str String
>               | Tuple [Expr]
>               | Record [(String,Expr)]
>               | VoidS
>               deriving (Eq,Show,Data,Typeable,Generic) 

> extractInt :: Scientific -> Maybe Int
> extractInt n = case floatingOrInteger n of
>                          (Right x :: Either Float Integer) -> Just $ fromIntegral x
>                          Left _ -> Nothing


> data VariantDecl = VariantDecl String [String]
>                  deriving (Eq,Show,Data,Typeable,Generic) 

