
> module Desugar (desugar) where

> import qualified Syntax as S
> import qualified InterpreterSyntax as I
> import Data.Generics.Uniplate.Data
> --import Data.Data (Data)
> --import Debug.Trace

approach:
when you get a [s.stmt], wrap it in a block - implicit block
but don't add more blocks as you go

what does
a = 5
when a = b: ccc end
desugar to?
let a = 5: block: when a = b: ccc end end

desugar takes a single statement

desugarStatements takes a list of statements and makes them into a
single expression using a block. designed to desugar a top level
so it will do a let* for all the bindings (todo)

> desugarStatements :: [S.Stmt] -> Either String I.Expr
> desugarStatements [] = Right $ I.Iden "nothing"
> desugarStatements z = desugarStatements' [] z
>   where
>       desugarStatements' [] [] = Right $ I.Iden "nothing"
>       desugarStatements' [x@(I.Block{})] [] = Right x
>       desugarStatements' acc [] = Right $ I.Block $ reverse acc
>       desugarStatements' _acc [S.LetStmt {}] = Left "Let can't be last statement in block" --desugar (S.Let [(s,e)] $ Iden "nothing")
>       desugarStatements' acc (S.LetStmt s e : ss) = do
>           x <- desugarStatements [S.StExpr (S.Let [(s,e)] (S.Block ss))]
>           desugarStatements' (x:acc) []
>       desugarStatements' acc (s:ss) = do
>           x <- desugar s
>           desugarStatements' (x:acc) ss
> -- todo: fix this
> -- if you have a file with only lets
> -- it should do a let* in nothing for all the binds, instead of erroring
> -- but revisit this desugar when start doing exports and multiple files
> --desugarStatements [S.LetStmt s e] = desugar [S.StExpr (Let [(s,e)] (S.Iden "nothing"))]
> --desugarStatements (S.LetStmt s e : ss) = 
> --desugarStatements (s:ss) = desugar s : desugarStatements
> --S.LetStmt s e : ss) = 



> desugar :: S.Stmt -> Either String I.Expr
> desugar (S.StExpr e) = desugarExpr e
> desugar (S.When c t) =
>     desugarExpr (S.If [(c, S.Block [S.StExpr t
>                                    ,S.StExpr $ S.Iden "nothing"])]
>                  (Just (S.Iden "nothing")))
> desugar (S.LetStmt {}) = Left "block ending with let" -- S.Let s <$> desugarExpr e

 > desugar' [S.LetStmt s e] = Left "block ending with let"
 > desugar' (S.LetStmt s e:ss) = desugar (S.StExpr (S.Let [(s, e)] (S.Block ss)))



> desugarExpr :: S.Expr -> Either String I.Expr
> --desugarExpr S.True = Right $ I.True
> --desugarExpr S.False = Right $ I.False
> desugarExpr (S.Num n) = Right $ I.Num n
> desugarExpr (S.Str s) = Right $ I.Str s
> desugarExpr (S.Iden i) = Right $ I.Iden i
> desugarExpr (S.Parens e) = desugarExpr e
> desugarExpr (S.Ask b e) = desugarExpr (S.If b e)

> desugarExpr (S.If [] Nothing) = Right $ I.App (I.Iden "raise") (I.Str "no branches matched")
> desugarExpr (S.If [] (Just e)) = desugarExpr e
> desugarExpr (S.If ((c,t):xs) els) = I.If <$> desugarExpr c <*> desugarExpr t <*> desugarExpr (S.If xs els)


> desugarExpr (S.App f xs) = do
>     f' <- desugarExpr f
>     xs' <- mapM desugarExpr xs
>     let r _ [] = Left "internal error app with no args"
>         r g [y] = Right $ I.App g y
>         r g (y:ys) = r (I.App g y) ys
>     r f' xs'

> desugarExpr (S.UnaryMinus e) = desugarExpr (S.App (S.Iden "*") [S.Num (-1), e])

> desugarExpr (S.BinOp a op b) = desugarExpr (S.App (S.Iden op) [a,b])

> desugarExpr (S.Lam [] _) = Left $ "lambda with no args"
> desugarExpr (S.Lam [x] bdy) = I.Lam x <$> desugarExpr bdy
> desugarExpr (S.Lam (x:xs) bdy) = I.Lam x <$> desugarExpr (S.Lam xs bdy)

> desugarExpr (S.Let [] bdy) = desugarExpr bdy
> desugarExpr (S.Let ((n,lbdy):ls) bdy) = do
>     bdy' <- desugarExpr (S.Let ls bdy)
>     lbdy' <- desugarExpr lbdy
>     Right $ I.App (I.Lam n bdy') lbdy'

> desugarExpr (S.LetRec [(f,(S.Lam as bdy))] ex) =
>    let f' = f ++ "XXX" -- todo: find a better way to generate a unique name
>        newBdy = patchCalls bdy -- replace calls to f(xxx) with calls to f(f,xxx)
>        newe =  (S.Let [(f', S.Lam (f:as) newBdy)]
>                    (S.Let [(f, S.Lam as (S.App (S.Iden f') (map S.Iden (f':as))))] ex))
>   in desugarExpr newe
>  where
>      -- todo: this will replace too much - how to fix it?
>      patchCalls = transformBi $ \x -> case x of
>          S.App (S.Iden fx) args -> S.App (S.Iden fx) (S.Iden fx : args)
>          _ -> x
>      

letrec f(a) = ...
->
let f'(g,a) = ... with f replaced with g
    f(a) = f'(f',a)

letrec f = lam (a): bdy end
->
let fz = lam (f,a): bdy' end
    f = lam(a): f'(f',a) end


> desugarExpr (S.Block ss) = desugarStatements ss -- I.Block <$> mapM desugar ss
