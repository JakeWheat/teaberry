
> {-# LANGUAGE ScopedTypeVariables #-}
> module Pretty (prettyExpr
>               ,prettyStmts
>               ,prettyModule
>               ) where

> import Prelude hiding ((<>))
> import Text.PrettyPrint (render, text, (<>), (<+>), empty, parens,
>                          nest, Doc, punctuate, comma, sep, {-quotes,-}
>                          doubleQuotes,
>                          {-braces, ($$), ($+$),-} vcat)

> import Syntax (Stmt(..)
>               ,Expr(..)
>               ,Selector(..)
>               ,VariantDecl(..)
>               ,Pat(..)
>               ,PatName(..)
>               ,Stmt(..)
>               ,Shadow(..)
>               ,Module(..)
>               ,PreludeStmt(..)
>               ,ProvideItem(..)
>               ,ImportSource(..)
>               ,Ref(..))
> import Scientific (showScientific)

> prettyExpr :: Expr -> String
> prettyExpr = render . expr

> prettyStmts :: [Stmt] -> String
> prettyStmts = render . stmts

> prettyModule :: Module -> String
> prettyModule = render . pmodule


> expr :: Expr -> Doc
> expr (Sel (Num n)) = text $ showScientific n
> expr (Sel (Text s)) = doubleQuotes (text s)
> expr (Sel (TupleSel es)) = text "{" <> nest 2 (xSep ";" (map expr es) <> text "}")
> expr (Sel (RecordSel flds)) = text "{" <> nest 2 (commaSep (map fld flds) <> text "}")
>   where
>     fld (n,e) = text n <> text ":" <+> expr e
> 
> expr (Iden n) = text n
> expr (Parens e) = parens (expr e)
> expr (If cs el) =
>     vcat (prettyCs cs ++ pel el ++ [text "end"])
>   where
>     prettyCs [] = []
>     prettyCs ((c,t):cs') = [text "if" <+> expr c <> text ":"
>                            ,nest 2 (expr t)]
>                            ++ concat (map prettyEx cs')
>     prettyEx (c,t) = [text "else" <+> text "if" <+> expr c <> text ":"
>                      ,nest 2 (expr t)]
>     pel Nothing = []
>     pel (Just e) = [text "else:"
>                    ,nest 2 (expr e)]

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
> expr (Lam bs e) = vcat
>     [text "lam" <> parens (commaSep $ map patName bs) <> text ":"
>     ,nest 2 (expr e)
>     ,text "end"]
> expr (Let bs e) =
>     vcat [text "let" <+> nest 2 bs' <> text ":"
>          ,nest 2 (expr e)
>          ,text "end"]
>   where
>     bs' | [(n,v)] <- bs = binding n v
>         | otherwise = vcommaSep $ map (uncurry binding) bs
> expr (LetRec bs e) = vcat
>     [text "letrec" <+> nest 2 (commaSep $ map (uncurry binding) bs) <> text ":"
>     ,nest 2 (expr e)
>     ,text "end"]
> expr (Block ss) = vcat [text "block:", nest 2 (stmts ss), text "end"]

> expr (Construct e as) = text "[" <> expr e <> text ":" <+> nest 2 (commaSep $ map expr as) <> text "]"

> expr (TupleGet e n) = expr e <> text ".{" <> text (show n) <> text "}"
> expr (DotExpr e i) = expr e <> text "." <> text i

> expr (Cases ty e mats els) =
>     vcat
>     [text "cases" <> parens (text ty) <+> expr e <> text ":"
>     ,nest 2 $ vcat (map mf mats ++
>                 [maybe empty (\x -> text "|" <+> text "else" <+> text "=>" <+> expr x) els])
>     ,text "end"]
>   where
>     mf (p, e1) = text "|" <+> pat p <+> text "=>" <+> expr e1

> expr (UnboxRef e f) = expr e <> text "!" <> text f

> binding :: PatName -> Expr -> Doc
> binding n e =
>     patName n <+> text "=" <+> nest 2 (expr e)


> pat :: Pat -> Doc
> pat (IdenP pn) = patName pn
> pat (VariantP q c ps) = maybe empty (\a -> text a <> text ".") q
>                         <> text c <> parens (commaSep $ map pat ps)
> pat (AsP p pn) = pat p <+> text "as" <+> patName pn

> patName :: PatName -> Doc
> patName (PatName s p) = (case s of
>                              NoShadow -> empty
>                              Shadow -> text "shadow")
>                         <+> text p

> stmt :: Stmt -> Doc
> stmt (StExpr e) = expr e
> stmt (When c t) = text "when" <+> expr c <> text ":" <+> nest 2 (expr t) <+> text "end"

> stmt (LetDecl n e) = binding n e

> stmt (VarDecl pn e) = text "var" <+> patName pn <+> text "=" <+> expr e
> stmt (SetVar n e) = text n <+> text ":=" <+> nest 2 (expr e)
> stmt (SetRef e fs) = expr e <> text "!{" <> commaSep (map f fs) <> text "}"
>   where
>     f (n,v) = text n <> text ":" <+> expr v

> stmt (RecDecl n e) = text "rec" <+> binding n e
> stmt (FunDecl pn as e w) = vcat
>      [text "fun" <+> patName pn <+> parens (commaSep $ map patName as) <> text ":"
>      ,nest 2 (expr e)
>      ,maybe empty whereBlock w
>      ,text "end"]

> stmt (DataDecl nm vs w ) =
>     vcat
>     [text "data" <+> text nm <+> text ":"
>     ,nest 2 (vcat $ map vf vs)
>     ,maybe empty whereBlock w
>     ,text "end"]
>   where
>       vf (VariantDecl vnm fs) = text "|" <+> text vnm <> case fs of
>                                                              [] -> empty
>                                                              _ -> parens (commaSep $ map f fs)
>       f (m, x) = (case m of
>                      Ref -> text "ref"
>                      _ -> empty)
>                  <+> text x

> stmt (Check nm ts) =
>     vcat [text "check" <+> maybe empty (doubleQuotes . text) nm <> text ":"
>          ,nest 2 (stmts ts)
>          ,text "end"]

> stmt (TPred e0 t pr e1) = expr e0 <+> text t <> parens (expr pr) <+> expr e1
> stmt (TPostfixOp e o) = expr e <+> text o

> stmts :: [Stmt] -> Doc
> stmts = vcat . map stmt



> whereBlock :: [Stmt] -> Doc
> whereBlock ts = vcat
>     [text "where:"
>     ,nest 2 (stmts ts)]

> commaSep :: [Doc] -> Doc
> commaSep ds = sep $ punctuate comma ds

> vcommaSep :: [Doc] -> Doc
> vcommaSep ds = vcat $ punctuate comma ds


> xSep :: String -> [Doc] -> Doc
> xSep x ds = sep $ punctuate (text x) ds

> pmodule :: Module -> Doc
> pmodule (Module prel sts) =
>     vcat (map preludeStmt prel ++ [stmts sts])

> preludeStmt :: PreludeStmt -> Doc
> preludeStmt (Provide pis) =
>     vcat [text "provide:"
>          ,nest 2 $ commaSep $ map provideItem pis
>          ,text "end"]
> preludeStmt (Include s) = text "include" <+> importSource s
> preludeStmt (IncludeFrom a pis) =
>     vcat [text "include" <+> text "from" <+> text a <> text ":"
>          ,nest 2 $ commaSep $ map provideItem pis
>          ,text "end"]
> preludeStmt (Import is a) = text "import" <+> importSource is <+> text "as" <+> text a
> preludeStmt (ImportNames nms is) = text "import" <+> commaSep (map text nms)
>                                   <+> text "from" <+> importSource is

> provideItem :: ProvideItem -> Doc
> provideItem ProvideAll = text "*"
> provideItem (ProvideName n) = text n
> provideItem (ProvideAlias n a) = text n <+> text "as" <+> text a

> importSource :: ImportSource -> Doc
> importSource (ImportSpecial nm as) = text nm <> parens (commaSep $ map (doubleQuotes . text) as)
> importSource (ImportName s) = text s
