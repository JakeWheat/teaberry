
The syntax which the interpreter uses

> module InterpreterSyntax where

> import Data.Scientific (Scientific)

> data Program = Program Expr
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
>           | Lam String Expr
>           | LamVoid Expr
>           | Let String Expr Expr
>           | AppHaskell String [Expr]
>           | Seq Expr Expr
>           | Box Expr
>           | SetBox String Expr
>           | LetDecl String Expr
>           deriving (Eq,Show)

> data Selector = Num Scientific
>               | Str String
>               | Variant String [(String,Expr)]
>               | VoidS
>               deriving (Eq,Show)
