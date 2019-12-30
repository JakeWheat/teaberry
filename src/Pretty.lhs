
> {-# LANGUAGE ScopedTypeVariables #-}
> module Pretty (prettyExpr
>               ,prettyStmts
>               ,prettyTestStmt
>               ) where

> import Data.Scientific (floatingOrInteger)

> import Prelude hiding ((<>))
> import Text.PrettyPrint (render, text, (<>), (<+>), empty, parens,
>                          nest, Doc, punctuate, comma, sep, {-quotes,-}
>                          doubleQuotes,
>                          {-braces, ($$), ($+$),-} vcat)

> import Syntax (Stmt(..), Expr(..), Selector(..), VariantDecl(..), Pat(..), TestStmt(..))

> prettyExpr :: Expr -> String
> prettyExpr = render . expr

> prettyStmts :: [Stmt] -> String
> prettyStmts = render . stmts

> prettyTestStmt :: TestStmt -> String
> prettyTestStmt = render . testStmt

> expr :: Expr -> Doc
> expr (Sel (Num n)) = text $ case floatingOrInteger n of
>                          (Right x :: Either Float Integer) -> show x
>                          Left _ ->  show n
> expr (Sel (Str s)) = doubleQuotes (text s)
> expr (Sel (Tuple es)) = text "{" <> nest 2 (xSep ";" (map expr es) <> text "}")
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

> expr (Construct e as) = text "[" <> expr e <> text ":" <+> nest 2 (commaSep $ map expr as) <> text "]"

> expr (TupleGet e n) = expr e <> text ".{" <> text (show n) <> text "}"
> expr (DotExpr e i) = expr e <> text "." <> text i

> expr (Cases ty e mats els) =
>     text "cases" <> parens (text ty) <+> expr e <> text ":"
>     <+> nest 2 (vcat (map mf mats ++
>                 [maybe empty (\x -> text "|" <+> text "else" <+> text "=>" <+> expr x) els]))
>     <+> text "end"
>   where
>     mf (p, e1) = text "|" <+> pat p <+> text "=>" <+> expr e1

> pat :: Pat -> Doc
> pat (IdenP p) = text p
> pat (CtorP c ps) = text c <> parens (commaSep $ map pat ps)
> pat (TupleP ps) = text "{" <> (xSep ";" $ map pat ps) <> text "}"

> stmt :: Stmt -> Doc
> stmt (StExpr e) = expr e
> stmt (When c t) = text "when" <+> expr c <> text ":" <+> nest 2 (expr t) <+> text "end"

> stmt (LetDecl n e) = text n <+> text "=" <+> nest 2 (expr e)

> stmt (VarDecl n e) = text "var" <+> text n <+> text "=" <+> nest 2 (expr e)
> stmt (SetVar n e) = text n <+> text ":=" <+> nest 2 (expr e)

> stmt (RecDecl n e) = text "rec" <+> text n <+> text "=" <+> nest 2 (expr e)
> stmt (FunDecl n as e w) =
>      text "fun" <+> text n <+> parens (commaSep $ map text as) <+> text ":"
>      <+> nest 2 (expr e)
>      <+> maybe empty whereBlock w
>      <+> text "end"

> stmt (DataDecl nm vs w ) =
>     text "data" <+> text nm <+> text ":"
>     <+> nest 2 (vcat $ map vf vs)
>     <+> maybe empty whereBlock w
>     <+> text "end"
>   where
>       vf (VariantDecl vnm fs) = text "|" <+> text vnm <+> parens (commaSep $ map text fs)

> stmt (Check nm ts) =
>     text "check" <+> maybe empty (doubleQuotes . text) nm <> text ":"
>     <+> nest 2 (testStmts ts)
>     <+> text "end"


> stmts :: [Stmt] -> Doc
> stmts = vcat . map stmt

> testStmts :: [TestStmt] -> Doc
> testStmts = vcat . map testStmt

> testStmt :: TestStmt -> Doc
> testStmt (TStmt st) = stmt st
> testStmt (TBinOp e0 op e1) = expr e0 <+> text op <+> expr e1
> testStmt (TPred e0 t pr e1) = expr e0 <+> text t <> parens (expr pr) <+> expr e1
> testStmt (TPostfixOp e o) = expr e <+> text o


> whereBlock :: [TestStmt] -> Doc
> whereBlock ts = text "where:" <+> nest 2 (testStmts ts)

> commaSep :: [Doc] -> Doc
> commaSep ds = sep $ punctuate comma ds

> xSep :: String -> [Doc] -> Doc
> xSep x ds = sep $ punctuate (text x) ds
