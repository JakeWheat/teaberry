
> module Desugar (desugar,sugar) where

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
>       desugarStatements' _acc [S.LetDecl {}] = Left "Let can't be last statement in block" --desugar (S.Let [(s,e)] $ Iden "nothing")
>       desugarStatements' acc (S.LetDecl s e : ss) = do
>           x <- desugarStatements [S.StExpr (S.Let [(s,e)] (S.Block ss))]
>           desugarStatements' (x:acc) []
>       desugarStatements' acc (s:ss) = do
>           x <- desugar s
>           desugarStatements' (x:acc) ss

todo:
check the errors
adjacent lets should be combined
don't create a block if there is only one statement left
is this worth it?



> desugar :: S.Stmt -> Either String I.Expr
> desugar (S.StExpr e) = desugarExpr e
> desugar (S.When c t) =
>     desugarExpr (S.If [(c, S.Block [S.StExpr t
>                                    ,S.StExpr $ S.Iden "nothing"])]
>                  (Just (S.Iden "nothing")))
> desugar (S.LetDecl {}) = Left "block ending with let" -- S.Let s <$> desugarExpr e

 > desugar' [S.LetStmt s e] = Left "block ending with let"
 > desugar' (S.LetStmt s e:ss) = desugar (S.StExpr (S.Let [(s, e)] (S.Block ss)))



> desugarExpr :: S.Expr -> Either String I.Expr
> --desugarExpr S.True = Right $ I.True
> --desugarExpr S.False = Right $ I.False
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
>     let r _ [] = Left "internal error app with no args"
>         r g [y] = Right $ I.App g y
>         r g (y:ys) = r (I.App g y) ys
>     r f' xs'

> desugarExpr (S.UnaryMinus e) = desugarExpr (S.App (S.Iden "*") [S.Sel $ S.Num (-1), e])

> desugarExpr (S.BinOp a op b) = desugarExpr (S.App (S.Iden op) [a,b])

> desugarExpr (S.Lam [] _) = Left $ "lambda with no args"
> desugarExpr (S.Lam [x] bdy) = I.Lam x <$> desugarExpr bdy
> desugarExpr (S.Lam (x:xs) bdy) = I.Lam x <$> desugarExpr (S.Lam xs bdy)

> desugarExpr (S.Let [] bdy) = desugarExpr bdy
> desugarExpr (S.Let ((n,lbdy):ls) bdy) = do
>     bdy' <- desugarExpr (S.Let ls bdy)
>     lbdy' <- desugarExpr lbdy
>     Right $ I.Let n lbdy' bdy' -- I.App (I.Lam n bdy') lbdy'

> {-desugarExpr (S.LetRec [(f,bdy)] ex) = do
>     bdy' <- desugarExpr bdy
>     ex' <- desugarExpr ex
>     return $ I.Let f (I.Fix (I.Lam f bdy')) ex'-}


letrec fact = \x -> bdy(x)
let fact = fix (\fact x -> bdy(x))

figure out why fix isn't working
  start by doing pretty printer
  and doing some trivial calls
  maybe it's evaluating something too early

letrec* in scheme:

(define (Y* . l)
   ((lambda (u) (u u))
    (lambda (p)
       (map (lambda (li) (lambda x (apply (apply li (p p)) x))) l))))

can this be typed easily?

just do the variable one for now, and see if can find a way to type it

- can easily write a desugar in haskell which passes the bindings in
   as variables, like the first version of letrec below

but one way to type check this is to build on a fix operator which is
   treated specially in the type checking. not sure how to do type
   checking without this

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


letrec f(a) = ...
->
let f'(g,a) = ... with f replaced with g
    f(a) = f'(f',a)

letrec f = lam (a): bdy end
->
let fz = lam (f,a): bdy' end
    f = lam(a): f'(f',a) end


> desugarExpr (S.Block ss) = desugarStatements ss -- I.Block <$> mapM desugar ss


TODO: make sure all of these will parse with the parser after being prettied

can also test that desugar . sugar . desugar === desugar

> sugar :: I.Expr -> S.Expr
> sugar (I.Sel (I.Num n)) = S.Sel (S.Num n)
> sugar (I.Sel (I.Str s)) = S.Sel (S.Str s)
> sugar (I.Iden i) = S.Iden i
> sugar (I.If c t e) = S.If [(sugar c, sugar t)] (Just (sugar e))

todo: do special cases for unary - and for binops which all look like
apps in the interpreter syntax, but have different syntax in the
regular syntax

> sugar (I.App f e) = S.App (sugar f) [sugar e]
> sugar (I.Lam a e) = S.Lam [a] (sugar e)
> sugar (I.Let n v e) = S.Let [(n, sugar v)] (sugar e)
> sugar (I.AppHaskell s es) = S.App (S.Iden ("haskell-" ++ s)) (map sugar es)
> sugar (I.Block es) = S.Block (map (S.StExpr . sugar) es)

