
> module ParseTests (parseExprExamples
>                   ,testParseExpr
>                   ,parseStmtExamples
>                   ,testParseStmt
>                   ,parseStmtsExamples
>                   ,testParseStmts) where
>
> import Syntax
> import Parse

> import Test.Tasty
> import Test.Tasty.HUnit

> parseExprExamples :: [(String, Expr)]
> parseExprExamples = [("a", Iden "a")
>                 ,(" a", Iden "a")
>                 ,("a ", Iden "a")
>                 ,(" a # comment", Iden "a")
>                 ,("a + b", BinOp (Iden "a") "+" (Iden "b"))
>                  -- todo: check this is the correct associativity
>                 ,("a + b + c", BinOp (Iden "a") "+" (BinOp (Iden "b") "+" (Iden "c")))
>                 ,(" a #| multiline\n\
>                   \comment |# + b", BinOp (Iden "a") "+" (Iden "b"))

>                 ,("true", Iden "true")
>                 ,("false", Iden "false")
>                 ,("3", Num 3)
>                 ,("3.3", Num 3.3)
>                 ,("\"String\"", Str "String")
>                 --,("\"St\\\"ri\\\"ng\"", Str "St\"ri\"ng")
>                 --,("'Str\"ing'", Str "Str\"ing")
>                 --,("```multiline\nstring```", Str "multiline\nstring")

>                ,("a(3)", App (Iden "a") [Num 3])

>                ,("a(3,4)", App (Iden "a") [Num 3, Num 4])

>                ,("- a", UnaryMinus (Iden "a"))

>                ,("(a)", Parens (Iden "a"))

>                ,("lam(x): x + 1 end", Lam ["x"] (BinOp (Iden "x") "+" (Num 1)))

>                ,("lam(x, y): x - y end"
>                 ,Lam ["x","y"] (BinOp (Iden "x") "-" (Iden "y")))

>                ,("lam(x, y): x - y end(1,2)"
>                 ,App (Lam ["x","y"] (BinOp (Iden "x") "-" (Iden "y")))
>                  [Num 1, Num 2])

todo: review all the whitespace rules that are being ignored

>               {-,("x = 3\n\
>                 \x + 4", Let [("x", Num 3)] (BinOp (Iden "x") "+" (Num 4)))

>               ,("x = 3\n\
>                 \y = 4\n\
>                 \x + y", Let [("x", Num 3)
>                              ,("y", Num 4)]
>                          (BinOp (Iden "x") "+" (Iden "y")))-}

>               ,("let x=3,y=4: x + y end", Let [("x", Num 3)
>                                               ,("y", Num 4)]
>                                           (BinOp (Iden "x") "+" (Iden "y")))
>               ,("let x=3: x + 4 end", Let [("x", Num 3)]
>                          (BinOp (Iden "x") "+" (Num 4)))

lam
todo: fun
todo: let simple variation

todo: tuples

>               ,("if a: b end", If [(Iden "a",Iden "b")] Nothing)
>               ,("if a: b else: c end", If [(Iden "a",Iden "b")] (Just (Iden "c")))
>               ,("if a: b else if c: d else: e end"
>                ,If [(Iden "a",Iden "b"),(Iden "c",Iden "d")] (Just (Iden "e")))

>               ,("ask: | otherwise: a end", Ask [] (Just (Iden "a")))


>               ,("ask:\n\
>                 \   | a == b then: c\n\
>                 \   | c == d then: e\n\
>                 \   | otherwise: f\n\
>                 \end"
>                 ,Ask [(BinOp (Iden "a") "==" (Iden "b"), Iden "c")
>                      ,(BinOp (Iden "c") "==" (Iden "d"), Iden "e")
>                      ] (Just (Iden "f")))

>               ,("ask:\n\
>                 \   | a == b then: c\n\
>                 \   | c == d then: e\n\
>                 \end"
>                 ,Ask [(BinOp (Iden "a") "==" (Iden "b"), Iden "c")
>                      ,(BinOp (Iden "c") "==" (Iden "d"), Iden "e")
>                      ] Nothing)

>               ,("block: a + 3 end"
>                ,Block [StExpr (BinOp (Iden "a") "+" (Num 3))])

>               ,("block:\n\
>                 \a = 5\n\
>                 \a + 3 end"
>                ,Block [LetStmt "a" (Num 5.0)
>                       ,StExpr (BinOp (Iden "a") "+" (Num 3))])

>                 ]


> parseStmtExamples :: [(String, Stmt)]
> parseStmtExamples =
>     [("when x == 3: 4 end"
>      ,When (BinOp (Iden "x") "==" (Num 3)) (Num 4))]

> parseStmtsExamples :: [(String, [Stmt])]
> parseStmtsExamples = [("", [])]


> testParseExpr :: (String,Expr) -> TestTree
> testParseExpr (src, ex) = testCase ("parseexpr " ++ src) $ do
>     case parseExpr "" src of
>         Left er -> error er
>         Right x -> assertEqual "" ex x

> testParseStmt :: (String,Stmt) -> TestTree
> testParseStmt (src, ex) = testCase ("parsestmt " ++ src) $ do
>     case parseStmt "" src of
>         Left er -> error er
>         Right x -> assertEqual "" ex x

> testParseStmts :: (String,[Stmt]) -> TestTree
> testParseStmts (src, ex) = testCase ("parsestmts " ++ src) $ do
>     case parseStmts "" src of
>         Left er -> error er
>         Right x -> assertEqual "" ex x
