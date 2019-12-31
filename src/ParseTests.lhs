
> module ParseTests (parseExprExamples
>                   ,testParseExpr
>                   ,parseStmtExamples
>                   ,testParseStmt
>                   ,parseProgramExamples
>                   ,testParseProgram
>                   ) where
>
> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

> import Syntax (Stmt(..), Expr(..), Selector(..), VariantDecl(..), Pat(..), TestStmt(..)
>               ,Program(..)
>               ,Provide(..)
>               ,ProvideTypes(..)
>               ,Import(..)
>               ,ImportSource(..))

> import Parse (parseExpr, parseStmt, parseProgram)
> import Pretty (prettyExpr, prettyStmts, prettyProgram)

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
>                 \end", Block[FunDecl "f" ["a"] (BinOp (Iden "a") "+" (num 1)) Nothing])


>               ,("block:\n\
>                 \fun f(a):\n\
>                 \  a = 1\n\
>                 \  a + 1\n\
>                 \end\n\
>                 \end", Block[FunDecl "f" ["a"] (Block [LetDecl "a" (num 1)
>                                                       ,StExpr $ BinOp (Iden "a") "+" (num 1)]) Nothing])


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
>                               ,VariantDecl "leaf" ["value"]] Nothing)

>     ,("data MyEnum:\n\
>       \  | node(left, right)\n\
>       \  | leaf\n\
>       \end", DataDecl "MyEnum" [VariantDecl "node" ["left", "right"]
>                               ,VariantDecl "leaf" []] Nothing)

>     ,("data MyEnum:\n\
>       \  | node(left, right)\n\
>       \  | leaf()\n\
>       \end", DataDecl "MyEnum" [VariantDecl "node" ["left", "right"]
>                               ,VariantDecl "leaf" []] Nothing)

>     ,("data Point:\n\
>       \  | pt(x, y)\n\
>       \end", DataDecl "Point" [VariantDecl "pt" ["x", "y"]] Nothing)

>     ,("data Point: pt(x, y) end"
>      ,DataDecl "Point" [VariantDecl "pt" ["x", "y"]] Nothing)

>     ,("data Point: pt() end"
>      ,DataDecl "Point" [VariantDecl "pt" []] Nothing)

>     ,("data Point: pt end"
>      ,DataDecl "Point" [VariantDecl "pt" []] Nothing)


>               ,("check \"a first block\":\n\
>                 \  5 is 5\n\
>                 \  4 is 5\n\
>                 \end"
>                ,Check (Just "a first block")
>                 [TBinOp (num 5) "is" (num 5)
>                 ,TBinOp (num 4) "is" (num 5)
>                 ])

>               ,("check:\n\
>                 \  6 is 7\n\
>                 \end"
>                ,Check Nothing
>                 [TBinOp (num 6) "is" (num 7)
>                 ])

>               ,("check \"all test syntax\":\n\
>                 \  expr1 is expr2\n\
>                 \  expr1 is_not expr2\n\
>                 \  expr1 is%(pred) expr2\n\
>                 \  expr1 is_not%(pred) expr2\n\
>                 \  expr satisfies pred\n\
>                 \  expr violates pred\n\
>                 \  expr raises \"exn_string\"\n\
>                 \  expr raises_other_than \"exn_string\"\n\
>                 \  expr does_not_raise\n\
>                 \  expr raises_satisfies pred\n\
>                 \  expr raises_violates pred\n\
>                 \end"
>                ,Check (Just "all test syntax")
>                 [TBinOp (Iden "expr1") "is" (Iden "expr2")
>                 ,TBinOp (Iden "expr1") "is_not" (Iden "expr2")
>                 ,TPred (Iden "expr1") "is%" (Iden "pred") (Iden "expr2")
>                 ,TPred (Iden "expr1") "is_not%" (Iden "pred") (Iden "expr2")
>                 ,TBinOp (Iden "expr") "satisfies" (Iden "pred")
>                 ,TBinOp (Iden "expr") "violates" (Iden "pred")
>                 ,TBinOp (Iden "expr") "raises" (Sel $ Str "exn_string")
>                 ,TBinOp (Iden "expr") "raises_other_than" (Sel $ Str "exn_string")
>                 ,TPostfixOp (Iden "expr") "does_not_raise"
>                 ,TBinOp (Iden "expr") "raises_satisfies" (Iden "pred")
>                 ,TBinOp (Iden "expr") "raises_violates" (Iden "pred")
>                 ])

>               ,("data Point:\n\
>                 \  | pt(x, y)\n\
>                 \where:\n\
>                 \  a_pt = pt(1,2)\n\
>                 \  is_Point(a_pt) is true\n\
>                 \end"
>                ,DataDecl "Point" [VariantDecl "pt" ["x", "y"]]
>                 (Just [TStmt (LetDecl "a_pt" (App (Iden "pt") [num 1, num 2]))
>                       ,TBinOp (App (Iden "is_Point") [Iden "a_pt"]) "is" (Iden "true")]))
>
>               ,("fun double(n):\n\
>                 \  n + n\n\
>                 \where:\n\
>                 \  double(10) is 20\n\
>                 \  double(15) is 30\n\
>                 \end"
>                ,FunDecl "double" ["n"] (BinOp (Iden "n") "+" (Iden "n"))
>                 (Just [TBinOp (App (Iden "double") [num 10]) "is" (num 20)
>                      ,TBinOp (App (Iden "double") [num 15]) "is" (num 30)]))


>     ]
>  where
>      num = Sel . Num

> parseProgramExamples :: [(String,Program)]
> parseProgramExamples =
>     [("\"hello\""
>      ,Program Nothing Nothing [] [StExpr $ Sel $ Str "hello"])

>     ,("provide {\n\
>       \  x : x\n\
>       \}\n\
>       \end\n\
>       \5"
>      ,Program (Just $ Provide [("x","x")]) Nothing [] [StExpr $ Sel $ Num 5])

>     ,("provide {\n\
>       \  x : x,\n\
>       \  y : z\n\
>       \}\n\
>       \end\n\
>       \5"
>      ,Program (Just $ Provide [("x","x")
>                               ,("y", "z")])
>       Nothing [] [StExpr $ Sel $ Num 5])

>     ,("provide *\n\
>       \5"
>      ,Program (Just ProvideAll)
>       Nothing [] [StExpr $ Sel $ Num 5])


>     ,("provide_types {\n\
>       \  Foo:: Foo,\n\
>       \  FooBar:: FooBar\n\
>       \}\n\
>       \5"
>      ,Program Nothing
>       (Just $ ProvideTypes [("Foo", "Foo"),("FooBar", "FooBar")]) [] [StExpr $ Sel $ Num 5])

>     ,("provide_types *\n\
>       \5"
>      ,Program Nothing
>       (Just ProvideTypesAll) [] [StExpr $ Sel $ Num 5])

>     ,("provide *\n\
>       \provide_types *\n\
>       \5"
>      ,Program (Just ProvideAll)
>       (Just ProvideTypesAll) [] [StExpr $ Sel $ Num 5])

>     ]


> testParseX :: (Eq a, Show a) => (FilePath -> String -> Either String a)
>            -> (a -> String)
>            -> (String,a)
>            -> T.TestTree
> testParseX parse pretty (src, ex) = T.testCase src $ do
>     case parse "" src of
>         Left er -> error er
>         Right x -> do
>             T.assertEqual "" ex x
>             let src1 = pretty x
>             case parse "" src1 of
>                 Left er -> error er
>                 Right x1 -> T.assertEqual "ppp" ex x1


> testParseExpr :: (String,Expr) -> T.TestTree
> testParseExpr t = testParseX parseExpr prettyExpr t

> testParseStmt :: (String,Stmt) -> T.TestTree
> testParseStmt t = testParseX parseStmt (prettyStmts . (:[])) t

> testParseProgram :: (String,Program) -> T.TestTree
> testParseProgram t = testParseX parseProgram prettyProgram t

