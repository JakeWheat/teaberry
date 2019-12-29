
> module ParseTests (parseExprExamples
>                   ,testParseExpr
>                   ,parseStmtExamples
>                   ,testParseStmt
>                   ,parseStmtsExamples
>                   ,testParseStmts) where
>
> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

> import Syntax (Stmt(..), Expr(..), Selector(..), VariantDecl(..), Pat(..))
> import Parse (parseExpr, parseStmt, parseStmts)
> import Pretty (prettyExpr, prettyStmts)

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
>                 ,("3", num 3)
>                 ,("3.3", num 3.3)
>                 ,("\"String\"", Sel $ Str "String")
>                 --,("\"St\\\"ri\\\"ng\"", Str "St\"ri\"ng")
>                 --,("'Str\"ing'", Str "Str\"ing")
>                 --,("```multiline\nstring```", Str "multiline\nstring")

>                ,("a(3)", App (Iden "a") [num 3])

>                ,("a(3,4)", App (Iden "a") [num 3, num 4])

>                ,("- a", UnaryMinus (Iden "a"))

>                ,("(a)", Parens (Iden "a"))

>                ,("lam(x): x + 1 end", Lam ["x"] (BinOp (Iden "x") "+" (num 1)))

>                ,("lam(x, y): x - y end"
>                 ,Lam ["x","y"] (BinOp (Iden "x") "-" (Iden "y")))

>                ,("lam(x, y): x - y end(1,2)"
>                 ,App (Lam ["x","y"] (BinOp (Iden "x") "-" (Iden "y")))
>                  [num 1, num 2])

todo: review all the whitespace rules that are being ignored

>               ,("let x=3,y=4: x + y end", Let [("x", num 3)
>                                               ,("y", num 4)]
>                                           (BinOp (Iden "x") "+" (Iden "y")))
>               ,("let x=3: x + 4 end", Let [("x", num 3)]
>                          (BinOp (Iden "x") "+" (num 4)))


>               ,("f()", App (Iden "f") [])


>               ,("let x = f(): x end", Let [("x", App (Iden "f") [])]
>                          (Iden "x"))

>               ,("block:\n\
>                 \  fun f(a): a + 1 end\n\
>                 \end", Block[FunDecl "f" ["a"] (BinOp (Iden "a") "+" (num 1))])


>               ,("block:\n\
>                 \fun f(a):\n\
>                 \  a = 1\n\
>                 \  a + 1\n\
>                 \end\n\
>                 \end", Block[FunDecl "f" ["a"] (Block [LetDecl "a" (num 1)
>                                                       ,StExpr $ BinOp (Iden "a") "+" (num 1)])])


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
>                ,Block [StExpr (BinOp (Iden "a") "+" (num 3))])

>               ,("block:\n\
>                 \a = 5\n\
>                 \a + 3 end"
>                ,Block [LetDecl "a" (num 5.0)
>                       ,StExpr (BinOp (Iden "a") "+" (num 3))])

>               ,("lam() : 1 end", Lam [] (num 1))


>               ,("block:\n\
>                 \  rec fact = lam(x):\n\
>                 \    if x == 0: 1\n\
>                 \    else: x * fact(x - 1)\n\
>                 \    end\n\
>                 \  end\n\
>                 \  fact(5)\n\
>                 \end"

>                ,Block [RecDecl "fact"
>                       $ Lam ["x"] $
>                               If [(BinOp (Iden "x") "==" (num 0), num 1)]
>                               (Just (BinOp (Iden "x") "*" (App (Iden "fact") [BinOp (Iden "x") "-" (num 1)])))
>                       ,StExpr (App (Iden "fact") [num 5])])



>               ,("[list: 1,2,3]", Construct (Iden "list") [num 1, num 2, num 3])
>               ,("[list: ]", Construct (Iden "list") [])

>               --,("let {x; y}: {1; 2} in x + y end"
>               -- ,Let [(TuplePat ["x","y"], Tuple [num 1, num 2])] (BinOp (Iden "x" "+" (Iden "y"))))

>               -- needs some work in the parser
>               -- ,("{1; 2;}", Sel $ Tuple [num 1, num 2])
>               ,("{1; 2}", Sel $ Tuple [num 1, num 2])

>               ,("[list: {\"a\"; 1}, {\"b\"; 2}, {\"c\"; 3}]"
>                ,Construct (Iden "list") [Sel $ Tuple [Sel $ Str "a",num 1]
>                                         ,Sel $ Tuple [Sel $ Str "b",num 2]
>                                         ,Sel $ Tuple [Sel $ Str "c",num 3]])
>               ,("t.{1}", TupleGet (Iden "t") 1)

>               ,("a.x", DotExpr (Iden "a") "x")

>               ,("cases(List) [list: 1,2,3]:\n\
>                 \  | empty => \"empty\"\n\
>                 \  | link(f, r) => \"link\"\n\
>                 \end"
>                ,Cases "List" (Construct (Iden "list") [num 1, num 2, num 3])
>                   [(IdenP "empty", Sel $ Str "empty")
>                   ,(CtorP "link" [IdenP "f", IdenP "r"], Sel $ Str "link")]
>                   Nothing)

>               ,("cases(List) [list: 1,2,3]:\n\
>                 \  | empty => \"empty\"\n\
>                 \  | else => \"else\"\n\
>                 \end"
>                ,Cases "List" (Construct (Iden "list") [num 1, num 2, num 3])
>                   [(IdenP "empty", Sel $ Str "empty")]
>                   (Just $ Sel $ Str "else"))

>               ,("cases(List) [list: {\"a\"; 1}, {\"b\"; 2}, {\"c\"; 3}]:\n\
>                 \  | empty => \"empty\"\n\
>                 \  | link({x;y}, r) => x\n\
>                 \  | else => \"else\"\n\
>                 \end"
>                ,Cases "List" (Construct (Iden "list") [Sel $ Tuple [Sel $ Str "a", num 1]
>                                                       ,Sel $ Tuple [Sel $ Str "b", num 2]
>                                                       ,Sel $ Tuple [Sel $ Str "c", num 3]])
>                   [(IdenP "empty", Sel $ Str "empty")
>                   ,(CtorP "link" [TupleP [IdenP "x", IdenP "y"], IdenP "r"], Iden "x")]
>                   (Just $ Sel $ Str "else"))


>                 ]
>  where
>      num = Sel . Num

> parseStmtExamples :: [(String, Stmt)]
> parseStmtExamples =
>     [("when x == 3: 4 end"
>      ,When (BinOp (Iden "x") "==" (Sel $ Num 3)) (Sel $ Num 4))
>     ,("var a = 5", VarDecl "a" (num 5))
>     ,("a := 6", SetVar "a" (num 6))
>     ,("data BTree:\n\
>       \  | node(value, left, right)\n\
>       \  | leaf(value)\n\
>       \end", DataDecl "BTree" [VariantDecl "node" ["value", "left", "right"]
>                               ,VariantDecl "leaf" ["value"]])

>     ,("data MyEnum:\n\
>       \  | node(left, right)\n\
>       \  | leaf\n\
>       \end", DataDecl "MyEnum" [VariantDecl "node" ["left", "right"]
>                               ,VariantDecl "leaf" []])

>     ,("data MyEnum:\n\
>       \  | node(left, right)\n\
>       \  | leaf()\n\
>       \end", DataDecl "MyEnum" [VariantDecl "node" ["left", "right"]
>                               ,VariantDecl "leaf" []])

>     ,("data Point:\n\
>       \  | pt(x, y)\n\
>       \end", DataDecl "Point" [VariantDecl "pt" ["x", "y"]])

>     ,("data Point: pt(x, y) end"
>      ,DataDecl "Point" [VariantDecl "pt" ["x", "y"]])

>     ,("data Point: pt() end"
>      ,DataDecl "Point" [VariantDecl "pt" []])

>     ,("data Point: pt end"
>      ,DataDecl "Point" [VariantDecl "pt" []])




>     ]
>  where
>      num = Sel . Num

> parseStmtsExamples :: [(String, [Stmt])]
> parseStmtsExamples = [("", [])]


> testParseExpr :: (String,Expr) -> T.TestTree
> testParseExpr (src, ex) = T.testCase ("parseexpr " ++ src) $ do
>     case parseExpr "" src of
>         Left er -> error er
>         Right x -> do
>             T.assertEqual "" ex x
>             let src1 = prettyExpr x
>             case parseExpr "" src1 of
>                 Left er -> error er
>                 Right x1 -> T.assertEqual "ppp" ex x1

> testParseStmt :: (String,Stmt) -> T.TestTree
> testParseStmt (src, ex) = T.testCase ("parsestmt " ++ src) $ do
>     case parseStmt "" src of
>         Left er -> error er
>         Right x -> do
>             T.assertEqual "" ex x
>             let src1 = prettyStmts [x]
>             case parseStmt "" src1 of
>                 Left er -> error er
>                 Right x1 -> T.assertEqual "ppp" ex x1

> testParseStmts :: (String,[Stmt]) -> T.TestTree
> testParseStmts (src, ex) = T.testCase ("parsestmts " ++ src) $ do
>     case parseStmts "" src of
>         Left er -> error er
>         Right x -> do
>             T.assertEqual "" ex x
>             let src1 = prettyStmts x
>             case parseStmts "" src1 of
>                 Left er -> error er
>                 Right x1 -> T.assertEqual "ppp" ex x1
