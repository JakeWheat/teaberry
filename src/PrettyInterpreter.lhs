

> module PrettyInterpreter (prettyProgram
>                          ) where

> import qualified InterpreterSyntax as I
> import qualified Syntax as S
> import qualified Pretty as P

> prettyProgram :: I.Program -> String
> prettyProgram = P.prettyProgram . resugarProgram

> resugarProgram :: I.Program -> S.Program
> resugarProgram (I.Program e cs) =
>     S.Program Nothing Nothing [] (S.StExpr (resugarExpr e) : concat (map resugarCheckBlock cs))
>
> resugarExpr :: I.Expr -> S.Expr
> resugarExpr (I.Sel (I.Num n)) = S.Sel (S.Num n)
> resugarExpr (I.Sel (I.Str s)) = S.Sel (S.Str s)
> resugarExpr (I.Sel (I.Tuple s)) = S.Sel (S.Tuple $ map resugarExpr s)
> resugarExpr (I.Sel I.VoidS) = S.Sel S.VoidS

> resugarExpr (I.Iden s) = S.Iden s
> resugarExpr (I.If a b c) = S.If [(resugarExpr a,resugarExpr b)] (Just $ resugarExpr c)
> resugarExpr (I.App e e1) = S.App (resugarExpr e) [resugarExpr e1]
> resugarExpr (I.Lam nm e) = S.Lam [nm] $ resugarExpr e
> resugarExpr (I.LamVoid e) = S.Lam [] $ resugarExpr e
> resugarExpr (I.Let nm v e) = S.Let [(nm,resugarExpr v)] (resugarExpr e)
> resugarExpr (I.AppHaskell nm e) = S.App (S.Iden nm) $ map resugarExpr e
> resugarExpr (I.Seq e0 e1) = S.Block [S.StExpr $ resugarExpr e0, S.StExpr $ resugarExpr e1]
> resugarExpr (I.Box e) = S.App (S.Iden "$box") [resugarExpr e]
> resugarExpr (I.SetBox s e) =  S.Block [S.SetVar s (resugarExpr e)]
> resugarExpr (I.LetDecl nm e) = S.Block [S.LetDecl nm (resugarExpr e)]

> resugarCheckBlock :: I.CheckBlock -> [S.Stmt]
> resugarCheckBlock (I.CheckBlock nm e) =
>     [S.Check (Just nm) [S.StExpr $ resugarExpr e]]

 data CheckBlock = CheckBlock String Expr
