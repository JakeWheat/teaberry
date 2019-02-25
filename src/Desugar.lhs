
> module Desugar (desugar) where

> import qualified Syntax as S
> import qualified InterpreterSyntax as I

> desugar :: S.Expr -> Either String I.Expr
> desugar S.True = Right $ I.True
> desugar S.False = Right $ I.False
> desugar (S.Num n) = Right $ I.Num n
> desugar (S.Str s) = Right $ I.Str s
> desugar (S.Iden i) = Right $ I.Iden i
> desugar (S.Parens e) = desugar e
> desugar (S.Ask b e) = desugar (S.If b e)

> desugar (S.If [] Nothing) = Right $ I.App (I.Iden "raise") (I.Str "no branches matched")
> desugar (S.If [] (Just e)) = desugar e
> desugar (S.If ((c,t):xs) els) = I.If <$> desugar c <*> desugar t <*> desugar (S.If xs els)


> desugar (S.App f xs) = do
>     f' <- desugar f
>     xs' <- mapM desugar xs
>     let r _ [] = Left "internal error app with no args"
>         r g [y] = Right $ I.App g y
>         r g (y:ys) = r (I.App g y) ys
>     r f' xs'

> desugar (S.UnaryMinus e) = desugar (S.App (S.Iden "*") [S.Num (-1), e])

> desugar (S.BinOp a op b) = desugar (S.App (S.Iden op) [a,b])

> desugar (S.Lam [] _) = Left $ "lambda with no args"
> desugar (S.Lam [x] bdy) = I.Lam x <$> desugar bdy
> desugar (S.Lam (x:xs) bdy) = I.Lam x <$> desugar (S.Lam xs bdy)

> desugar (S.Let [] bdy) = desugar bdy
> desugar (S.Let ((n,lbdy):ls) bdy) = do
>     bdy' <- desugar (S.Let ls bdy)
>     lbdy' <- desugar lbdy
>     Right $ I.App (I.Lam n bdy') lbdy'


Expr = True
           | False
           | Num Scientific
           | Str String
           | Iden String
           | Parens Expr

           | TupleCtor [Expr]
           | TupleGet Expr Integer
 
           | If [(Expr,Expr)] (Maybe Expr)
           | Ask [(Expr,Expr)] (Maybe Expr)

           | App Expr [Expr]
           | UnaryMinus Expr
           | BinOp Expr String Expr

           | Lam [String] Expr
           | Fun [String] Expr
           | Let [(String,Expr)] Expr
