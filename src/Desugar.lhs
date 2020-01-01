
> module Desugar (desugarProgram,desugarExpr) where

> import Data.Generics.Uniplate.Data (transformBi)
> import Data.Maybe (catMaybes, mapMaybe)

> import Control.Monad.RWS (RWST(..), runRWST)
> import Control.Monad.Except (Except, runExcept, throwError)


> import qualified Syntax as S
> import qualified InterpreterSyntax as I
> import qualified Pretty as P


> desugarProgram :: S.Program -> Either String I.Program
> desugarProgram (S.Program Nothing Nothing [] stmts) =
>     case runExcept (runRWST f () ()) of
>         Left e -> Left e
>         Right (result, _store, _log) -> Right result
>   where
>     f :: RWST () [()] () (Except String) I.Program
>     f = do

doing this weird create [I.Stmt], then seqify, doesn't seem like a
good way to do it

>         (a,b) <- desugarStmts' stmts
>         pure $ I.Program (seqify a) b

> desugarExpr :: S.Expr -> Either String I.Expr
> desugarExpr e =
>     case runExcept (runRWST (desugarExpr' e) () ()) of
>         Left x -> Left x
>         Right (result, _store, _log) -> Right result


> type DesugarStack = RWST () [()] () (Except String)


> desugarStmts' :: [S.Stmt] -> DesugarStack ([I.Expr], [I.CheckBlock])
> desugarStmts' (s:ss) | Just s' <- convRec s = do

rules for fun and rec:
when a fun or rec is seen, it will collect subsequent funs and recs
(both) and then desugar them all together as one letrec

>     -- get all the immediately following recursive defs
>     let (adddecls,ss') = spanMaybe convRec ss
>     -- desugar them to regular let together
>     defs <- desugarRecs (s' : adddecls)
>     -- desugar to interpreter syntax
>     idecls <- desugarStmts' $ map (uncurry S.LetDecl) defs
>     addStuff idecls <$> desugarStmts' ss'
>   where
>     convRec (S.FunDecl nm as bdy whr) = Just (nm, (S.Lam as bdy))
>     convRec (S.RecDecl nm e) = Just (nm, e)
>     convRec _ = Nothing -- x = Left $ "unexpected non fun/rec decl: " ++ show x
> desugarStmts' (s:ss) = addStuff <$> desugarStmt s <*> desugarStmts' ss
> desugarStmts' [] = pure ([],[])

> spanMaybe :: (t -> Maybe a) -> [t] -> ([a], [t])
> spanMaybe _ xs@[] =  ([], xs)
> spanMaybe p xs@(x:xs') = case p x of
>     Just y  -> let (ys, zs) = spanMaybe p xs' in (y : ys, zs)
>     Nothing -> ([], xs)

> addStuff :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
> addStuff (a,b) (a', b') = (a ++ a', b ++ b')

> liftStmt :: I.Expr -> ([I.Expr], [I.CheckBlock])
> liftStmt s = ([s],[])

> desugarStmt :: S.Stmt -> DesugarStack ([I.Expr], [I.CheckBlock])

> desugarStmt  (S.StExpr x@(S.BinOp e0 "is" e1)) = do
>   let p = P.prettyExpr x
>   y <- desugarIs "unknown" p e0 e1
>   desugarStmt y
> 
> desugarStmt (S.StExpr e) = liftStmt <$> desugarExpr' e
> desugarStmt (S.When c t) = liftStmt <$>
>     desugarExpr' (S.If [(c, S.Block [S.StExpr t
>                                    ,S.StExpr $ S.Iden "nothing"])]
>                     (Just (S.Iden "nothing")))

> desugarStmt (S.LetDecl nm e) = liftStmt <$> I.LetDecl nm <$> desugarExpr' e

> desugarStmt (S.FunDecl nm as bdy whr) =
>     desugarStmt (S.RecDecl nm (S.Lam as bdy))
> desugarStmt (S.RecDecl nm e) = do
>    defs <- desugarRecs [(nm,e)]
>    desugarStmts' $ map (uncurry S.LetDecl) defs

> desugarStmt (S.VarDecl n e) = liftStmt <$> (I.LetDecl n . I.Box) <$> desugarExpr' e
> desugarStmt (S.SetVar n e) = liftStmt <$> I.SetBox n <$> desugarExpr' e

> desugarStmt (S.Check nm sts) = do
>     let nm' = maybe "anonymous check block" id nm
>     (sts'',x) <- desugarStmts' sts
>     case x of
>         [] -> pure ()
>         _ -> throwError $ "internal error: test in desugared test"
>     let sts''' = seqify sts''
>     pure ([], [I.CheckBlock nm' (sts''')])
>     

> desugarIs :: String -> String -> S.Expr -> S.Expr -> DesugarStack S.Stmt
> desugarIs blockName syn e e1 = do
>     let --syn = P.prettyStmt x
>         -- blockName = cn
>         mys = S.StExpr $ S.Block
>          [S.LetDecl "bn" $ S.Sel $ S.Str blockName
>          ,S.LetDecl "tst" $ S.Sel $ S.Str syn
>          ,S.LetDecl "v0" e
>          ,S.LetDecl "v1" e1
>          ,S.StExpr $ S.If [(S.App (S.Iden "==") [S.Iden "v0", S.Iden "v1"]
>               ,S.App (S.Iden "log_test_pass") [S.Iden "bn", S.Iden "tst"])]
>              (Just $ S.App (S.Iden "log_test_fail")
>              [S.Iden "bn", S.Iden "tst"
>              ,str "Values not equal:\n" `plus` app "torepr" [S.Iden "v0"]
>               `plus` str "\n" `plus` app "torepr" [S.Iden "v1"]])
>          ,S.StExpr $ S.Iden "false"]
>     pure mys
>   where
>       plus a b = S.BinOp a "+" b
>       str = S.Sel . S.Str
>       app nm es = S.App (S.Iden nm) es

block:
  tst = "5 is 6"
  v0 = 5
  v1 = 6
  if v0 == v1:
    log_test_pass(tst)
  else:
    log_test_failure(tst, "Values not equal:\n" + torepr(v0) + "\n" + torepr(v1))
  end
  false
end


--------------------------------------


> desugarExpr' :: S.Expr -> DesugarStack I.Expr
> desugarExpr' (S.Sel (S.Num n)) = pure $ I.Sel (I.Num n)
> desugarExpr' (S.Sel (S.Str s)) = pure $ I.Sel (I.Str s)
> desugarExpr' (S.Iden i) = pure $ I.Iden i
> desugarExpr' (S.Parens e) = desugarExpr' e
> desugarExpr' (S.Ask b e) = desugarExpr' (S.If b e)

> desugarExpr' (S.If [] Nothing) = pure $ I.App (I.Iden "raise") (I.Sel $ I.Str "no branches matched")
> desugarExpr' (S.If [] (Just e)) = desugarExpr' e
> desugarExpr' (S.If ((c,t):xs) els) = I.If <$> desugarExpr' c <*> desugarExpr' t <*> desugarExpr' (S.If xs els)

> desugarExpr' (S.App f xs) = do
>     f' <- desugarExpr' f
>     xs' <- mapM desugarExpr' xs
>     let r g [] = pure $ I.App g (I.Sel $ I.VoidS)
>         r g [y] = pure $ I.App g y
>         r g (y:ys) = r (I.App g y) ys
>     r f' xs'

> desugarExpr' (S.UnaryMinus e) = desugarExpr' (S.App (S.Iden "*") [S.Sel $ S.Num (-1), e])

> desugarExpr' (S.BinOp a op b) = desugarExpr' (S.App (S.Iden op) [a,b])

> desugarExpr' (S.Lam [] bdy) = I.LamVoid <$> desugarExpr' bdy
> desugarExpr' (S.Lam [x] bdy) = I.Lam x <$> desugarExpr' bdy
> desugarExpr' (S.Lam (x:xs) bdy) = I.Lam x <$> desugarExpr' (S.Lam xs bdy)

> desugarExpr' (S.Let [] bdy) = desugarExpr' bdy
> desugarExpr' (S.Let ((n,lbdy):ls) bdy) = do
>     bdy' <- desugarExpr' (S.Let ls bdy)
>     lbdy' <- desugarExpr' lbdy
>     pure $ I.Let n lbdy' bdy'



> desugarExpr' (S.LetRec fs ex) = do
>    defs <- desugarRecs fs
>    {-trace (prettyExpr (S.Let defs ex)) -}
>    desugarExpr' (S.Let defs ex)

> desugarExpr' (S.Block ss) = do
>     (ss', x) <- desugarStmts' ss
>     case x of
>         [] -> pure ()
>         _ -> throwError "tests not at the top level are not supported"
>     pure $ seqify ss'

> desugarExpr' (S.Sel (S.Tuple ts)) = do
>     ts' <- mapM desugarExpr' ts
>     pure $ I.Sel $ I.Tuple ts'

> desugarExpr' (S.TupleGet e i) = do
>     desugarExpr' (S.App (S.Iden "tupleget") [e, S.Sel $ S.Num $ fromIntegral i])


> desugarExpr' x = error $ "desugarExpr': " ++ show x

> seqify :: [I.Expr] -> I.Expr
> seqify [] = I.Sel I.VoidS
> seqify [e] = e
> seqify (e:es) = I.Seq e $ seqify es


rec in blocks:

This is the canonical rewrite which I think says what works and what doesn't
work in pyret:

(letrec ([v e] ...) b...)
->
(let ([v "some dummy value"] ...)
  (set! v e)
  ...
  b)


a non lambda binding doesn't do 'rec', it can only
  refer to bindings before it in the list
a lambda binding can refer to later defined bindings, including non
  lambda ones


what about doubling up the non-lambda bindings to 0 arg lambdas
and seeing if this can make it work more nicely

letrec
  x = y,
  y = 1:
  x + y
end

->

letrec
  x' = lam() : y',
  y' = lam() : 1,
  x = x'(),
  y = y'():
  x + y
end
then desugar using the usual algo, making sure the x and y bindings
 come at the end

here is a case to cover, a recursive value:
rec tr = node("rec", lam(): tr end, lam(): tr end)

without the lambdas, is this only possible in a lazy language?
check the graphs section of papl

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

the current rewrite is like this, it has limitations compared to pyret:
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
  abc = factXXX(factXXX, 5)
  abc
end


> desugarRecs :: [(String,S.Expr)] -> DesugarStack [(String,S.Expr)]
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

