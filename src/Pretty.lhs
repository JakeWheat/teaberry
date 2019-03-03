

> module Pretty (prettyExpr
>               ,prettyStmts
>               ) where


> import Syntax

> import Prelude hiding ((<>))

> import Text.PrettyPrint (render, text, (<>), (<+>), empty, parens,
>                          nest, Doc, punctuate, comma, sep, quotes,
>                          doubleQuotes,
>                          braces, ($$), ($+$),vcat)

> prettyExpr :: Expr -> String
> prettyExpr = render . expr

> prettyStmts :: [Stmt] -> String
> prettyStmts = render . stmts


> expr :: Expr -> Doc
> expr (Sel (Num n)) = text $ show n
> expr (Sel (Str s)) = doubleQuotes (text s)
> expr (Iden n) = text n
> expr (Parens e) = parens (expr e)
> expr (If cs el) =
>     vcat (prettyCs cs ++ pel el ++ [text "end"])
>   where
>     prettyCs [] = []
>     prettyCs ((c,t):cs') = (text "if" <+> expr c <> text ":"
>                            <+> nest 2 (expr t))
>                            : map prettyEx cs'
>     prettyEx (c,t) = text "else" <+> text "if" <+> expr c <> text ":"
>                      <+> nest 2 (expr t)
>     pel Nothing = []
>     pel (Just e) = [text "else:" <+> nest 2 (expr e)]

> expr (Ask cs el) =
>     vcat [text "ask:", nest 2 (vcat (map prettyC cs ++ pel el)), text "end"]
>   where
>     prettyC (c,t) = text "|" <+> expr c <+> text "then:"
>                     <+> nest 2 (expr t)
>     pel Nothing = []
>     pel (Just e) = [text "|" <+> text "otherwise:" <+> nest 2 (expr e)]

> expr (App e es) = expr e <> parens (commaSep $ map expr es)
> expr (UnaryMinus e) = text "-" <> expr e
> expr (BinOp a op b) = expr a <+> text op <+> expr b
> expr (Lam ps e) = text "lam" <> parens (commaSep $ map text ps)
>     <> text ":" <+> nest 2 (expr e) <+> text "end"
> expr (Let bs e) = text "let" <+> nest 2 (commaSep $ map f bs)
>     <> text ":" <+> nest 2 (expr e) <+> text "end"
>   where
>     f (n,ne) = text n <+> text "=" <+> nest 2 (expr ne)
> expr (LetRec bs e) = text "letrec" <+> nest 2 (commaSep $ map f bs)
>     <> text ":" <+> nest 2 (expr e) <+> text "end"
>   where
>     f (n,ne) = text n <+> text "=" <+> nest 2 (expr ne)
> expr (Block ss) = vcat [text "block:", nest 2 (stmts ss), text "end"]

> stmt :: Stmt -> Doc
> stmt (StExpr e) = expr e
> stmt (When c t) = text "when" <+> expr c <> text ":" <+> nest 2 (expr t) <+> text "end"

> stmt (LetStmt n e) = text n <+> text "=" <+> nest 2 (expr e)


> stmts :: [Stmt] -> Doc
> stmts = vcat . map stmt


> commaSep :: [Doc] -> Doc
> commaSep ds = sep $ punctuate comma ds
