
Desugaring from the high level syntax to the interpreter syntax.

The most interesting part is the test desugaring (check:, example:,
where:)

and to a lesser extent letrec (and multiple rec/fun declarations)


> {-# LANGUAGE TupleSections #-}
> module Desugar (desugarProgram,desugarExpr) where

> import Data.Generics.Uniplate.Data (transformBi)
> import Data.Maybe (catMaybes, mapMaybe)

> import Control.Monad.RWS (RWST(..), runRWST, ask, local,state)
> import Control.Monad.Except (Except, runExcept, throwError)

> --import Debug.Trace (trace)

> import qualified Syntax as S
> import qualified InterpreterSyntax as I
> import qualified Pretty as P

------------------------------------------------------------------------------

= api

> desugarProgram :: S.Program -> Either String I.Program
> desugarProgram (S.Program Nothing Nothing [] stmts) =
>     case runExcept (runRWST f defaultDesugarReader defaultDesugarStore) of
>         Left e -> Left e
>         Right (result, _store, _log) -> Right result
>   where

doing this weird create [I.Stmt], then seqify, doesn't seem like a
good way to do it

>     f :: RWST DesugarReader [()] DesugarStore (Except String) I.Program
>     f = (I.Program . seqify) <$> desugarStmts stmts

> desugarProgram x = error $ "Desugar: desugarProgram " ++ show x


> desugarExpr :: S.Expr -> Either String I.Expr
> desugarExpr e =
>     case runExcept (runRWST (desugarExpr' e) defaultDesugarReader defaultDesugarStore) of
>         Left x -> Left x
>         Right (result, _store, _log) -> Right result

------------------------------------------------------------------------------

= types for the desugarer monad

> type DesugarStack = RWST DesugarReader [()] DesugarStore (Except String)

> data DesugarReader = DesugarReader
>     {inCheckBlockID :: Maybe Int -- just iff in a check block
>     }
>     deriving (Eq,Show)

> defaultDesugarReader :: DesugarReader
> defaultDesugarReader = DesugarReader Nothing

> data DesugarStore = DesugarStore
>     {nextCheckBlockNameID :: Int -- the id for the next anonymous check block name
>     ,nextCheckBlockID :: Int
>     }
>     deriving (Eq,Show)

> defaultDesugarStore :: DesugarStore
> defaultDesugarStore = DesugarStore 1 0

------------------------------------------------------------------------------

desugar a block

doesn't yet do the top level the way it is supposed to, pending
research on a better letrec than the usual scheme one

> desugarStmts :: [S.Stmt] -> DesugarStack [I.Expr]
> desugarStmts (s:ss) | Just s' <- convRec s = do

rules for fun and rec:
when a fun or rec is seen, it will collect subsequent funs and recs
(both) and then desugar them all together as one letrec

>     -- get all the immediately following recursive defs
>     let (addDecls',ss') = spanMaybe convRec ss
>         (addDecls, whrs') = unzip (s' : addDecls')
>         whrs :: [(Maybe String, [S.Stmt])]
>         whrs = map (\(a,b) -> (Just a, b)) $ catMaybes whrs'
>     -- desugar them to regular let together
>     defs <- desugarRecs addDecls
>     -- do the check blocks, not sure it's right to not interleave them
>     chks <- desugarStmts $ map (uncurry S.Check) whrs
>     -- desugar to interpreter syntax
>     idecls <- desugarStmts $ map S.LetDecl defs
>     (++) (idecls ++ chks) <$> desugarStmts ss'
>   where
>     convRec (S.FunDecl nm as bdy whr) = Just (S.Binding S.NoShadow (S.IdenP nm) (S.Lam as bdy), fmap (nm,) whr)
>     convRec (S.RecDecl b) = Just (b, Nothing)
>     convRec _ = Nothing

> desugarStmts (s:ss) = (++) <$> desugarStmt s <*> desugarStmts ss
> desugarStmts [] = pure []


> desugarStmt :: S.Stmt -> DesugarStack [I.Expr]

> desugarStmt (S.StExpr x@(S.BinOp e0 "is" e1)) = do
>   let p = P.prettyExpr x
>   y <- desugarIs p e0 e1
>   desugarStmt y
> 
> desugarStmt (S.StExpr e) = (:[]) <$> desugarExpr' e
> desugarStmt (S.When c t) = (:[]) <$>
>     desugarExpr' (S.If [(c, S.Block [S.StExpr t
>                                    ,S.StExpr $ S.Iden "nothing"])]
>                     (Just (S.Iden "nothing")))

> desugarStmt (S.LetDecl (S.Binding _ (S.IdenP nm) e)) = (:[]) <$> I.LetDecl nm <$> desugarExpr' e

> desugarStmt (S.FunDecl nm as bdy whr) = do
>     a <- desugarStmt (S.RecDecl (S.Binding S.NoShadow (S.IdenP nm) (S.Lam as bdy)))
>     case whr of
>         Nothing -> pure a
>         Just w -> (++) a <$> desugarStmt (S.Check (Just nm) w)
>     
> desugarStmt (S.RecDecl b) = do
>    defs <- desugarRecs [b]
>    desugarStmts $ map S.LetDecl defs

> desugarStmt (S.VarDecl (S.Binding _ (S.IdenP n) e)) = (:[]) <$> (I.LetDecl n . I.Box) <$> desugarExpr' e
> desugarStmt (S.SetVar n e) = (:[]) <$> I.SetBox n <$> desugarExpr' e

------------------------------------------------------------------------------

desugaring check blocks:

a check block is desugared to this:

add-tests(lam():
  checkblockid = n # unique number, and the variable name checkblock id should be unique (todo)
  log-check-block(checkblockid, "blockname")
  {desuged statements in the check block}
  VoidV # a top level check block has no value
end

todo: a non top level check block can use something like this:

expr
check:
...
end
->
shadow tmpvar = expr
add-tests(lam():
   ...
   # instead of ending with VoidV:
   tmpvar
end

this isn't needed for nested where:, but is for a nested
check/examples if they are at the end of a block

surprising behaviour in pyret:

block:
  y = 2
  y
  check "nested":
    y is 2
  end
end

runs the test and returns 2

block:
  y = 2
  check "nested":
    y is 2
  end
end

this succeeds, runs the test, and outputs nothing

block:
  y = 2
  #check "nested":
  #  y is 2
  #end
end

this fails with 'a block cannot end with a binding'


> desugarStmt (S.Check nm sts) = do
>     nm' <- case nm of
>                   Just x -> pure x
>                   Nothing -> do
>                       i <- state (\st -> (nextCheckBlockNameID st
>                                          ,st {nextCheckBlockNameID = nextCheckBlockNameID st + 1}))
>                       pure $ "check-block-" ++ show i
>     checkblockid <- state (\st -> (nextCheckBlockID st
>                                   ,st {nextCheckBlockID  = nextCheckBlockID st + 1}))
>     let checkblockidnm = "checkblockid"
>         blockNameVal = S.Sel $ S.Str nm'
>         checkblockidval = S.Sel $ S.Num $ fromIntegral checkblockid
>         blk = [S.LetDecl (S.Binding S.NoShadow (S.IdenP checkblockidnm) checkblockidval)
>                 ,S.StExpr $ S.App (S.Iden "log-check-block") [S.Iden checkblockidnm
>                                                              ,blockNameVal]
>               ] ++ sts ++ [S.StExpr $ S.Sel S.VoidS]
>         blockWrap =
>             [S.StExpr $ S.App (S.Iden "add-tests") $ [S.Lam [] $ S.Block blk]]
>     sts' <- local (\x -> x {inCheckBlockID = Just checkblockid}) $ desugarStmts blockWrap
>     pure [seqify sts']
>     

> desugarStmt x = error $ "Desugar desugarStmt " ++ show x

an "is" test desugars to

block:
  shadow v0 = aexpr
  shadow v1 = bexpr # todo: change to tuple bind to avoid bexpr
                    # capturing the shadowed v0
  shadow name = "aexpr is bexpr"
  if v0 == v1:
    log-test-pass(checkblockid, name)
  else:
    shadow failmsg = "Values not equal:\n" + torepr(v0) + "\n" + torepr(v1)
    log-test-fail(checkblockid, name, failmsg)
  end
end

> desugarIs :: String -> S.Expr -> S.Expr -> DesugarStack S.Stmt
> desugarIs syn e e1 = do
>     x <- inCheckBlockID <$> ask
>     checkBlockID <- maybe (throwError $ "'is' outside of checkblock")
>                     (pure . S.Sel . S.Num . fromIntegral) x
>     let mys = S.StExpr $ S.Block
>                    [{-S.StExpr $ S.App (S.Iden "print") [S.Sel $ S.Str $ "Desugar test enter"]
>                    ,-}S.LetDecl (S.Binding S.Shadow (S.IdenP "v0") e)
>                    ,S.LetDecl (S.Binding S.Shadow (S.IdenP "v1") e1)
>                    ,S.LetDecl (S.Binding S.Shadow (S.IdenP "name") $ S.Sel $ S.Str syn)
>                    ,S.StExpr $ S.If [(S.App (S.Iden "==") [S.Iden "v0", S.Iden "v1"]
>                                    ,S.App (S.Iden "log-test-pass") [checkBlockID, S.Iden "name"])]
>                        (Just $ S.Block
>                         [S.LetDecl (S.Binding S.Shadow (S.IdenP "failmsg")
>                                     (str "Values not equal:\n" `plus` app "torepr" [S.Iden "v0"]
>                                               `plus` str "\n" `plus` app "torepr" [S.Iden "v1"]))
>                         ,S.StExpr $ S.App (S.Iden "log-test-fail")
>                          [checkBlockID, S.Iden "name", S.Iden "failmsg"]])]
>     pure mys
>   where
>       plus a b = S.BinOp a "+" b
>       str = S.Sel . S.Str
>       app nm es = S.App (S.Iden nm) es


--------------------------------------


> desugarExpr' :: S.Expr -> DesugarStack I.Expr
> desugarExpr' (S.Sel (S.Num n)) = pure $ I.Sel (I.Num n)
> desugarExpr' (S.Sel (S.Str s)) = pure $ I.Sel (I.Str s)
> desugarExpr' (S.Sel S.VoidS) = pure $ I.Sel I.VoidS
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

"short circuiting" and and or

> desugarExpr' (S.BinOp a "and" b) =
>     desugarExpr' (S.If [(S.App (S.Iden "==") [a, S.Iden "true"], b)]
>                   (Just $ S.Iden "false"))

> desugarExpr' (S.BinOp a "or" b) =
>     desugarExpr' (S.If [(S.App (S.Iden "==") [a, S.Iden "true"], S.Iden "true")]
>                      (Just b))


> desugarExpr' (S.BinOp a op b) = desugarExpr' (S.App (S.Iden op) [a,b])

> desugarExpr' (S.Lam [] bdy) = I.LamVoid <$> desugarExpr' bdy
> desugarExpr' (S.Lam [x] bdy) = I.Lam x <$> desugarExpr' bdy
> desugarExpr' (S.Lam (x:xs) bdy) = I.Lam x <$> desugarExpr' (S.Lam xs bdy)

> desugarExpr' (S.Let [] bdy) = desugarExpr' bdy
> desugarExpr' (S.Let (S.Binding _ (S.IdenP n) lbdy : ls) bdy) = do
>     bdy' <- desugarExpr' (S.Let ls bdy)
>     lbdy' <- desugarExpr' lbdy
>     pure $ I.Let n lbdy' bdy'



> desugarExpr' (S.LetRec fs ex) = do
>    defs <- desugarRecs fs
>    {-trace (prettyExpr (S.Let defs ex)) -}
>    desugarExpr' (S.Let defs ex)

> desugarExpr' (S.Block ss) = do
>     ss' <- desugarStmts ss
>     pure $ seqify ss'

> desugarExpr' (S.Sel (S.Tuple ts)) = do
>     ts' <- mapM desugarExpr' ts
>     pure $ I.Sel $ I.Variant "tuple" "tuple" (zip nms ts')
>   where
>     nms = map show [(0::Int)..]

> desugarExpr' (S.TupleGet e i) = do
>     desugarExpr' (S.App (S.Iden "variant-field-get") [e, S.Sel $ S.Str $ show i])


> desugarExpr' x = error $ "desugarExpr': " ++ show x

turn a list of expressions into a nested seq value

> seqify :: [I.Expr] -> I.Expr
> seqify [] = I.Sel I.VoidS
> seqify [e] = e
> seqify (e:es) = I.Seq e $ seqify es

------------------------------------------------------------------------------

rec in blocks:

This is the canonical rewrite from scheme which I think says what
works and what doesn't work in vanilla pyret:

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


> desugarRecs :: [S.Binding] -> DesugarStack [S.Binding]
> desugarRecs rs =
>     let
>         recnms = flip mapMaybe rs (\r -> case r of
>                                           S.Binding _ (S.IdenP f) (S.Lam {}) -> Just f
>                                           _ -> Nothing)
>         recmap = zip recnms recnms
>         recnms' = map (++ "XXX") recnms
>         recmap' = zip recnms recnms'
>         mkrec (S.Binding s (S.IdenP f) (S.Lam as bdy)) =
>             let f' = f ++ "XXX"
>                 bdy' = patchCalls recmap recnms bdy
>             in (Just (S.Binding S.NoShadow (S.IdenP f') (S.Lam (recnms ++ as) bdy'))
>                ,Just (S.Binding s (S.IdenP f) (S.Lam as (S.App (S.Iden f') (map S.Iden (recnms' ++ as))))))
>         mkrec (S.Binding s f e) = (Nothing
>                                   ,Just (S.Binding s f (patchCalls recmap' recnms' e)))
>         (a,b) = unzip $ map mkrec rs
>         x = catMaybes (a ++ b)
>     in pure x
>  where
>      --showit :: [(String, S.Expr)] -> String
>      --showit ss = intercalate "\n" $ map (\(a,b) -> a ++ " = " ++ prettyExpr b) ss
>      patchCalls mp ids = transformBi $ \x -> case x of
>          S.App (S.Iden fx) args | Just fx' <- lookup fx mp -> S.App (S.Iden fx') (map S.Iden ids ++ args)
>          _ -> x

------------------------------------------------------------------------------

utils

> spanMaybe :: (t -> Maybe a) -> [t] -> ([a], [t])
> spanMaybe _ xs@[] =  ([], xs)
> spanMaybe p xs@(x:xs') = case p x of
>     Just y  -> let (ys, zs) = spanMaybe p xs' in (y : ys, zs)
>     Nothing -> ([], xs)



------------------------------------------------------------------------------


Top level design notes

The top level will be desugared to a let rec
this isn't like other implicit let recs in blocks, it doesn't need
definitions to be next to each other - all definitions and statements
in the entire top level will 'participate' in the same letrec

rec a = ...

b = ...

rec c = ...

a and b and c will be part of the same letrec

check blocks 'participate' in the letrec, along with statements - have
 to figure out how to make this work

shadow doesn't work in a letrec for another var in the letrec:

rec a = 5
shadow rec a = 6
or any other variation is an error
_except_ at the top level

this is to support this at the top level:

a = 5

...

shadow a = 6

are bindings the only kind of redefinition that can happen?

a way to support this is roughly:
if idens are shadowed, rename to remove the shadowing
then, move the declarations first and the statements second
then it should all work
problem is, if a closure refers to a var and it gets updated
  this is ok for check blocks, but what about other things?
  will it work out ok?
  if var updates get moved to the bottom
  and all the non decl statements keep their relative order
  will everything work out?


this is only important for learning, an industrial only language
_could_ disallow this too, but if we want the top level to double
time as a script/implicit block and a top level for a more structured
program, this is a viable compromise (another one would be to have a
flag to switch which - e.g top level desugaring works like blocks,
shadow allowed for top level scripts, or top level works like a
regular letrec for not script-y programs, and shadow is not allowed
at the top level at all


