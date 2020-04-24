

> module PrettyInterpreter (prettyProgram
>                          ,prettyExpr
>                          ) where

> import qualified InterpreterSyntax as I
> import qualified Syntax as S
> import qualified Pretty as P

> prettyProgram :: I.Program -> String
> prettyProgram = P.prettyProgram . resugarProgram

> prettyExpr :: I.Expr -> String
> prettyExpr = P.prettyExpr . resugarExpr

> resugarProgram :: I.Program -> S.Program
> resugarProgram (I.Program e) =
>     S.Program [] [S.StExpr (resugarExpr e)]
>
> resugarExpr :: I.Expr -> S.Expr
> resugarExpr (I.Sel (I.Num n)) = S.Sel (S.Num n)

> resugarExpr (I.Sel (I.Str s)) = S.Sel (S.Str $ escape s)
>   where
>     escape [] = []
>     escape ('\n':xs) = '\\':'n':escape xs
>     escape (x:xs) = x : escape xs
>
> 
> resugarExpr (I.Sel (I.Variant nm s)) = S.App (S.Iden nm) $ map (resugarExpr . snd) s
> resugarExpr (I.Sel I.NothingS) = S.Sel S.NothingS

> resugarExpr (I.Iden s) = S.Iden s
> resugarExpr (I.If a b c) = S.If [(resugarExpr a,resugarExpr b)] (Just $ resugarExpr c)

todo: resugaring multiple arg apps and lams has a key readability benefit

> resugarExpr (I.LamVoid e) = S.Lam [] $ resugarExpr e
> resugarExpr (I.AppHaskell nm e) = S.App (S.Iden nm) $ map resugarExpr e


todo: desugar these:
tupleget
dotexpr

> resugarExpr (I.App (I.App (I.Iden o) e0) e1) | isBinop o =
>     S.BinOp (resugarExpr e0) o (resugarExpr e1)
>   where
>     isBinop x = all (`elem` "=<>+*-/") x
>                 || x `elem` ["and", "or"]

convert nested single arg apps into multi arg apps

> resugarExpr (I.App e e1) = f e [resugarExpr e1]
>   where
>     f (I.App x y) es = f x (resugarExpr y:es)
>     f x es = S.App (resugarExpr x) es


> resugarExpr (I.Lam nm e) = f [nm] e
>   where
>     f nms (I.Lam nm' e') = f (nm':nms) e'
>     f nms e' = S.Lam (map (S.IdenP S.NoShadow) $ reverse nms) $ resugarExpr e'

> resugarExpr (I.Let nm v e) = f [(nm,resugarExpr v)] e
>   where
>     f bs (I.Let nm' v' e') = f ((nm',resugarExpr v'):bs) e'
>     f bs e' = S.Let (map (\(a,b) -> S.Binding (S.IdenP S.NoShadow a) b) $ reverse bs) $ resugarExpr e'

> resugarExpr (I.Unbox x) = S.App (S.Iden "unbox") [resugarExpr x]


convert a sequence of seqs to a flat block

> resugarExpr (I.Seq e0 e1) =
>     let sqs = unwrap e0 : getSqs e1
>     in S.Block sqs
>   where
>     getSqs (I.Seq a b) = unwrap a : getSqs b
>     getSqs e = [unwrap e]
>     unwrap (I.SetBox s e) = S.StExpr $ S.App (S.Iden "setbox") [resugarExpr s,resugarExpr e]
>     unwrap (I.LetDecl nm e) = S.LetDecl $ S.Binding (S.IdenP S.NoShadow nm) (resugarExpr e)
>     unwrap x = S.StExpr $ resugarExpr x

> resugarExpr (I.Box e) = S.App (S.Iden "$box") [resugarExpr e]

adding a block here is wrong, it's a hack

> resugarExpr (I.SetBox (I.Iden s) e) =  S.Block [S.SetVar s (resugarExpr e)]
> resugarExpr (I.LetDecl nm e) = S.Block [S.LetDecl $ S.Binding (S.IdenP S.NoShadow nm) (resugarExpr e)]
