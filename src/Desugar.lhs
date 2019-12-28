
> module Desugar (desugarStmts,desugarStmt,desugarExpr) where

> import qualified Syntax as S
> import qualified InterpreterSyntax as I
> import Data.Generics.Uniplate.Data
> --import Data.Data (Data)
> --import Debug.Trace


> desugarStmts :: [S.Stmt] -> Either String [I.Stmt]
> desugarStmts (s:ss) = (:) <$> desugarStmt s <*> desugarStmts ss
> desugarStmts [] = pure []

> desugarStmt :: S.Stmt -> Either String I.Stmt
> desugarStmt (S.StExpr e) = I.StExpr <$> desugarExpr e
> desugarStmt (S.When c t) = I.StExpr <$> 
>     desugarExpr (S.If [(c, S.Block [S.StExpr t
>                                    ,S.StExpr $ S.Iden "nothing"])]
>                     (Just (S.Iden "nothing")))
> desugarStmt (S.LetDecl nm e) = I.LetDecl nm <$> desugarExpr e


> desugarExpr :: S.Expr -> Either String I.Expr
> desugarExpr (S.Sel (S.Num n)) = Right $ I.Sel (I.Num n)
> desugarExpr (S.Sel (S.Str s)) = Right $ I.Sel (I.Str s)
> desugarExpr (S.Iden i) = Right $ I.Iden i
> desugarExpr (S.Parens e) = desugarExpr e
> desugarExpr (S.Ask b e) = desugarExpr (S.If b e)

> desugarExpr (S.If [] Nothing) = Right $ I.App (I.Iden "raise") (I.Sel $ I.Str "no branches matched")
> desugarExpr (S.If [] (Just e)) = desugarExpr e
> desugarExpr (S.If ((c,t):xs) els) = I.If <$> desugarExpr c <*> desugarExpr t <*> desugarExpr (S.If xs els)


todo: special case for fix which looks like normal App in the regular syntax

> desugarExpr (S.App f xs) = do
>     f' <- desugarExpr f
>     xs' <- mapM desugarExpr xs
>     let r g [] = Right $ I.AppVoid g
>         r g [y] = Right $ I.App g y
>         r g (y:ys) = r (I.App g y) ys
>     r f' xs'

> desugarExpr (S.UnaryMinus e) = desugarExpr (S.App (S.Iden "*") [S.Sel $ S.Num (-1), e])

> desugarExpr (S.BinOp a op b) = desugarExpr (S.App (S.Iden op) [a,b])

> desugarExpr (S.Lam [] bdy) = I.LamVoid <$> desugarExpr bdy
> desugarExpr (S.Lam [x] bdy) = I.Lam x <$> desugarExpr bdy
> desugarExpr (S.Lam (x:xs) bdy) = I.Lam x <$> desugarExpr (S.Lam xs bdy)

> desugarExpr (S.Let [] bdy) = desugarExpr bdy
> desugarExpr (S.Let ((n,lbdy):ls) bdy) = do
>     bdy' <- desugarExpr (S.Let ls bdy)
>     lbdy' <- desugarExpr lbdy
>     Right $ I.Let n lbdy' bdy'


> desugarExpr (S.LetRec [(f,(S.Lam as bdy))] ex) =
>    let f' = f ++ "XXX" -- todo: find a better way to generate a unique name
>                        -- need proper machinery to do this
>        newBdy = patchCalls bdy -- replace calls to f(xxx) with calls to f(f,xxx)
>        newe =  (S.Let [(f', S.Lam (f:as) newBdy)]
>                    (S.Let [(f, S.Lam as (S.App (S.Iden f') (map S.Iden (f':as))))] ex))
>   in desugarExpr newe
>  where
>      -- todo: this will replace too much - how to fix it?
>      patchCalls = transformBi $ \x -> case x of
>          S.App (S.Iden fx) args -> S.App (S.Iden fx) (S.Iden fx : args)
>          _ -> x

> desugarExpr (S.LetRec {}) = Left "only trivial letrec supported"

> desugarExpr (S.Block ss) = do
>     ss' <- mapM desugarStmt ss
>     let f :: [I.Stmt] -> Either String I.Expr
>         f [] = Left "empty block"
>         f [I.StExpr e] = pure e
>         f [I.LetDecl {}] = Left "block ends with decl"
>         f [a,b] = pure $ I.Seq a b
>         f (a:as) = I.Seq a <$> I.StExpr <$> f as
>     f ss'



letrec f(a) = ...
->
let f'(g,a) = ... with f replaced with g
    f(a) = f'(f',a)

letrec f = lam (a): bdy end
->
let fz = lam (f,a): bdy' end
    f = lam(a): f'(f',a) end




