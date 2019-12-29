
> module Desugar (desugarStmts,desugarExpr) where

> import Data.Generics.Uniplate.Data (transformBi)
> import Data.Maybe (catMaybes, mapMaybe)

> import qualified Syntax as S
> import qualified InterpreterSyntax as I

> desugarStmts :: [S.Stmt] -> Either String [I.Stmt]
> desugarStmts (s:ss) | isRec s = do

rules for fun and rec:
when a fun or rec is seen, it will collect subsequent funs and recs
(both) and then desugar them all together as one letrec

>     -- get all the immediately following recursive defs
>     let (adddecls,ss') = span isRec ss
>     -- desugar them to regular let together
>     defs <- desugarRecs =<< mapM splitRec (s:adddecls)
>     -- desugar to interpreter syntax
>     idecls <- desugarStmts $ map (uncurry S.LetDecl) defs
>     (++) idecls <$> desugarStmts ss'
>   where
>     isRec (S.FunDecl {}) = True
>     isRec (S.RecDecl {}) = True
>     isRec _ = False
>     splitRec (S.FunDecl nm as bdy) = pure (nm, (S.Lam as bdy))
>     splitRec (S.RecDecl nm e) = pure (nm, e)
>     splitRec x = Left $ "unexpected non fun/rec decl: " ++ show x
> desugarStmts (s:ss) = (++) <$> desugarStmt s <*> desugarStmts ss
> desugarStmts [] = pure []

> desugarStmt :: S.Stmt -> Either String [I.Stmt]

> desugarStmt (S.StExpr e) = (:[]) <$> I.StExpr <$> desugarExpr e
> desugarStmt (S.When c t) = (:[]) <$> I.StExpr <$> 
>     desugarExpr (S.If [(c, S.Block [S.StExpr t
>                                    ,S.StExpr $ S.Iden "nothing"])]
>                     (Just (S.Iden "nothing")))

> desugarStmt (S.LetDecl nm e) = (:[]) <$> I.LetDecl nm <$> desugarExpr e

> desugarStmt (S.FunDecl nm as bdy) =
>     desugarStmt (S.RecDecl nm (S.Lam as bdy))
> desugarStmt (S.RecDecl nm e) = do
>    defs <- desugarRecs [(nm,e)]
>    desugarStmts $ map (uncurry S.LetDecl) defs

> desugarStmt (S.VarDecl n e) = (:[]) <$> (I.LetDecl n . I.Box) <$> desugarExpr e
> desugarStmt (S.SetVar n e) = (:[]) <$> I.StExpr <$> I.SetBox n <$> desugarExpr e


--------------------------------------


> desugarExpr :: S.Expr -> Either String I.Expr
> desugarExpr (S.Sel (S.Num n)) = Right $ I.Sel (I.Num n)
> desugarExpr (S.Sel (S.Str s)) = Right $ I.Sel (I.Str s)
> desugarExpr (S.Iden i) = Right $ I.Iden i
> desugarExpr (S.Parens e) = desugarExpr e
> desugarExpr (S.Ask b e) = desugarExpr (S.If b e)

> desugarExpr (S.If [] Nothing) = Right $ I.App (I.Iden "raise") (I.Sel $ I.Str "no branches matched")
> desugarExpr (S.If [] (Just e)) = desugarExpr e
> desugarExpr (S.If ((c,t):xs) els) = I.If <$> desugarExpr c <*> desugarExpr t <*> desugarExpr (S.If xs els)

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



> desugarExpr (S.LetRec fs ex) = do
>    defs <- desugarRecs fs
>    {-trace (prettyExpr (S.Let defs ex)) -}
>    desugarExpr (S.Let defs ex)

> desugarExpr (S.Block ss) = do
>     ss' <- desugarStmts ss
>     let f :: [I.Stmt] -> Either String I.Expr
>         f [] = Left "empty block"
>         f [I.StExpr e] = pure e
>         f [I.LetDecl {}] = Left "block ends with decl"
>         f [a,b] = pure $ I.Seq a b
>         f (a:as) = I.Seq a <$> I.StExpr <$> f as
>     f ss'



rec in blocks:
the rule is:
a non lambda binding doesn't do 'rec', it can only
  refer to bindings before it in the list
a lambda binding can refer to later defined bindings, including non
  lambda ones

how exactly does this work and not work?
create all the non lambda bindings as vars
create the lambda bindings using this closure
update the vars one by one in order
  -> will get an error if refer to a var not yet initialized

TODO: implement this behaviour

other todo for letrec:
  generate unique names
  replace only the correct calls when fixing the bodies:
    don't descend when shadowed

letrec f(a) = ...
->
let f'(g,a) = ... with f replaced with g
    f(a) = f'(f',a)

letrec f = lam (a): bdy end
->
let fz = lam (f,a): bdy' end
    f = lam(a): f'(f',a) end


letrec f0 = lam (as0): bdy0
       ...
       fn = lam (asn): bdyn
in ex ->
  [bdyn' = bdyn replace(fn(x) with fn(f0,...,fN,x)]
  let [fn' = lam(f0,...,fn,asn) bdyn']
      [fn = lam (asn) : fn'(f0',...,fn',asn)]
  in ex


letrec
  fact = lam(n): if n == 1: 1 else: n * fact(n - 1) end end,
  abc = fact(5):
  abc
end
->
let
  factXXX = lam(fact, n): if n == 1.0: 1.0
                            else: n * fact(fact, n - 1.0)
                            end end,
  fact = lam(n): factXXX(factXXX, n) end,
  abc = fact(5): # or: abc = factXXX(factXXX, 5)
  abc
end

This doesn't work in pyret - don't try to support it now
letrec
  abc = fact(5),
  fact = lam(n): if n == 1: 1 else: n * fact(n - 1) end end:
  abc
end
->
let
  factXXX = lam(fact, n): if n == 1.0: 1.0
                            else: n * fact(fact, n - 1.0)
                            end end,
  abc = factXXX(factXXX, 5)
  fact = lam(n): factXXX(factXXX, n) end,
  abc
end


> desugarRecs :: [(String,S.Expr)] -> Either String [(String,S.Expr)]
> desugarRecs rs =
>     let
>         recnms = flip mapMaybe rs (\r -> case r of
>                                           (f, S.Lam {}) -> Just f
>                                           _ -> Nothing)
>         recmap = zip recnms recnms
>         recnms' = map (++ "XXX") recnms
>         recmap' = zip recnms recnms'
>         mkrec (f,S.Lam as bdy) =
>             let f' = f ++ "XXX"
>                 bdy' = patchCalls recmap recnms bdy
>             in (Just (f', S.Lam (recnms ++ as) bdy')
>                ,Just (f, S.Lam as (S.App (S.Iden f') (map S.Iden (recnms' ++ as)))))
>         mkrec (f,e) = (Nothing
>                       ,Just (f, patchCalls recmap' recnms' e))
>         (a,b) = unzip $ map mkrec rs
>         x = catMaybes (a ++ b)
>     in pure x
>  where
>      --showit :: [(String, S.Expr)] -> String
>      --showit ss = intercalate "\n" $ map (\(a,b) -> a ++ " = " ++ prettyExpr b) ss
>      patchCalls mp ids = transformBi $ \x -> case x of
>          S.App (S.Iden fx) args | Just fx' <- lookup fx mp -> S.App (S.Iden fx') (map S.Iden ids ++ args)
>          _ -> x

