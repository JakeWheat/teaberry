

> module PrettyInterpreter (prettyProgram
>                          ) where

> import qualified InterpreterSyntax as I
> import qualified Syntax as S
> import qualified Pretty as P

> prettyProgram :: I.Program -> String
> prettyProgram = P.prettyProgram . resugarProgram

> resugarProgram :: I.Program -> S.Program
> resugarProgram (I.Program e) =
>     S.Program Nothing Nothing [] [S.StExpr (resugarExpr e)]
>
> resugarExpr :: I.Expr -> S.Expr
> resugarExpr (I.Sel (I.Num n)) = S.Sel (S.Num n)
> resugarExpr (I.Sel (I.Str s)) = S.Sel (S.Str s)
> resugarExpr (I.Sel (I.Variant nm s)) = S.App (S.Iden nm) $ map (resugarExpr . snd) s
> resugarExpr (I.Sel I.VoidS) = S.Sel S.VoidS

> resugarExpr (I.Iden s) = S.Iden s
> resugarExpr (I.If a b c) = S.If [(resugarExpr a,resugarExpr b)] (Just $ resugarExpr c)

todo: resugaring multiple arg apps and lams has a key readability benefit

> resugarExpr (I.App e e1) = S.App (resugarExpr e) [resugarExpr e1]
> resugarExpr (I.Lam nm e) = S.Lam [nm] $ resugarExpr e
> resugarExpr (I.LamVoid e) = S.Lam [] $ resugarExpr e
> resugarExpr (I.Let nm v e) = S.Let [S.Binding (S.IdenP S.NoShadow nm) $ resugarExpr v] (resugarExpr e)
> resugarExpr (I.AppHaskell nm e) = S.App (S.Iden nm) $ map resugarExpr e


todo: try to convert a sequence of seqs to a flat block

> resugarExpr (I.Seq e0 e1) = S.Block [S.StExpr $ resugarExpr e0, S.StExpr $ resugarExpr e1]
> resugarExpr (I.Box e) = S.App (S.Iden "$box") [resugarExpr e]

adding a block here is wrong, it's a hack

> resugarExpr (I.SetBox s e) =  S.Block [S.SetVar s (resugarExpr e)]
> resugarExpr (I.LetDecl nm e) = S.Block [S.LetDecl $ S.Binding (S.IdenP S.NoShadow nm) (resugarExpr e)]
