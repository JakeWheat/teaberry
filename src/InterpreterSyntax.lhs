
The syntax which the interpreter uses

> module InterpreterSyntax where

> import Data.Scientific (Scientific)

> data Program = Program [Stmt] [CheckBlock]
>               deriving (Eq,Show)

TODO: add provides and imports
  is this really a program? or something else?
  maybe a program should be the collection with all the imports and
  provides present or something
  either way, this is two seperate things and they need different
  data types/names

> data Expr = Sel Selector
>           | Iden String
>           | If Expr Expr Expr
>           | App Expr Expr
>           | AppVoid Expr
>           | Lam String Expr
>           | LamVoid Expr
>           | Let String Expr Expr
>           | AppHaskell String [Expr]
>           | Seq Stmt Stmt
>           | Box Expr
>           | SetBox String Expr
>           --  | SetVar String Expr
>           deriving (Eq,Show)

> data Stmt = StExpr Expr
>           | LetDecl String Expr
>           deriving (Eq,Show)


> data Selector = Num Scientific
>               | Str String
>               deriving (Eq,Show)

> data CheckBlock = CheckBlock String [Stmt]
>               deriving (Eq,Show)


