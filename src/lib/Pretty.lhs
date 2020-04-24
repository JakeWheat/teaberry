
> {-# LANGUAGE ScopedTypeVariables #-}
> module Pretty (prettyExpr
>               ,prettyStmts
>               ,prettyProgram
>               ) where

> import Prelude hiding ((<>))
> import Text.PrettyPrint (render, text, (<>), (<+>), empty, parens,
>                          nest, Doc, punctuate, comma, sep, {-quotes,-}
>                          doubleQuotes,
>                          {-braces, ($$), ($+$),-} vcat)

> import Syntax (Stmt(..), Expr(..), Selector(..), VariantDecl(..), Pat(..), Stmt(..)
>               ,Binding(..)
>               ,Shadow(..)
>               ,Program(..)
>               ,PreludeItem(..)
>               ,ProvideItem(..)
>               ,ImportSource(..)
>               ,Ref(..)
>               ,extractInt)
>  

> prettyExpr :: Expr -> String
> prettyExpr = render . expr

> prettyStmts :: [Stmt] -> String
> prettyStmts = render . stmts

> prettyProgram :: Program -> String
> prettyProgram = render . program


> expr :: Expr -> Doc
> expr (Sel (Num n)) = text $ case extractInt n of
>                              Just x -> show x
>                              Nothing ->  show n
> expr (Sel (Str s)) = doubleQuotes (text s)
> expr (Sel (Tuple es)) = text "{" <> nest 2 (xSep ";" (map expr es) <> text "}")
> expr (Sel (Record flds)) = text "{" <> nest 2 (commaSep (map fld flds) <> text "}")
>   where
>     fld (n,e) = text n <> text ":" <+> expr e
> expr (Sel NothingS) = text "nothing"
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
>     [text "lam" <> parens (commaSep $ map pat bs) <> text ":"
>     ,nest 2 (expr e)
>     ,text "end"]
> expr (Let bs e) =
>     vcat [text "let" <+> nest 2 bs' <> text ":"
>          ,nest 2 (expr e)
>          ,text "end"]
>   where
>     bs' | [b] <- bs = binding b
>         | otherwise = vcommaSep $ map binding bs
> expr (LetRec bs e) = vcat
>     [text "letrec" <+> nest 2 (commaSep $ map binding bs) <> text ":"
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

> expr (Unbox e f) = expr e <> text "!" <> text f

> binding :: Binding -> Doc
> binding (Binding n e) =
>     pat n <+> text "=" <+> nest 2 (expr e)


> pat :: Pat -> Doc
> pat (IdenP s p) = (case s of
>                        NoShadow -> empty
>                        Shadow -> text "shadow")
>                   <+> text p
> pat (VariantP c ps) = text c <> parens (commaSep $ map pat ps)
> pat (TupleP ps) = text "{" <> (xSep ";" $ map pat ps) <> text "}"
> pat (AsP p s nm) =
>     pat p
>     <+> text "as"
>     <+> (case s of
>                 NoShadow -> empty
>                 Shadow -> text "shadow")
>     <+> text nm

> stmt :: Stmt -> Doc
> stmt (StExpr e) = expr e
> stmt (When c t) = text "when" <+> expr c <> text ":" <+> nest 2 (expr t) <+> text "end"

> stmt (LetDecl b) = binding b

> stmt (VarDecl b) = text "var" <+> binding b
> stmt (SetVar n e) = text n <+> text ":=" <+> nest 2 (expr e)
> stmt (SetRef e fs) = expr e <> text "!{" <> commaSep (map f fs) <> text "}"
>   where
>     f (n,v) = text n <> text ":" <+> expr v

> stmt (RecDecl b) = text "rec" <+> binding b
> stmt (FunDecl n as e w) = vcat
>      [text "fun" <+> text n <+> parens (commaSep $ map pat as) <> text ":"
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

> program :: Program -> Doc
> program (Program prel sts) =
>     vcat (map preludeItem prel ++ [stmts sts])

> preludeItem :: PreludeItem -> Doc
> preludeItem (Provide pis) =
>     vcat [text "provide:"
>          ,nest 2 $ commaSep $ map provideItem pis
>          ,text "end"]
> preludeItem (Include s) = text "include" <+> importSource s
> preludeItem (IncludeFrom a pis) =
>     vcat [text "include" <+> text "from" <+> text a <> text ":"
>          ,nest 2 $ commaSep $ map provideItem pis
>          ,text "end"]
> preludeItem (Import is a) = text "import" <+> importSource is <+> text "as" <+> text a
> preludeItem (ImportNames nms is) = text "import" <+> commaSep (map text nms)
>                                   <+> text "from" <+> importSource is

> provideItem :: ProvideItem -> Doc
> provideItem ProvideAll = text "*"
> provideItem (ProvideName n) = text n
> provideItem (ProvideAlias n a) = text n <+> text "as" <+> text a

> importSource :: ImportSource -> Doc
> importSource (ImportSpecial nm as) = text nm <> parens (commaSep $ map (doubleQuotes . text) as)
> importSource (ImportName s) = text s
