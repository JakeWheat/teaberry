
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

> import Syntax (Stmt(..), Expr(..), Selector(..), VariantDecl(..), Pat(..)
>               ,Shadow(..)
>               ,Binding(..)
>               ,Program(..)
>               ,Provide(..)
>               ,ProvideTypes(..)
>               ,Import(..)
>               ,ImportSource(..))

> import Parse (parseExpr, parseStmt, parseProgram)
> import Pretty (prettyExpr, prettyStmts, prettyProgram)

> parseExprExamples :: [(String, Expr)]
> parseExprExamples =
>     [("a", Iden "a")
>     ,(" a", Iden "a")
>     ,("a ", Iden "a")
>     ,(" a # comment", Iden "a")
>     ,("a + b", BinOp (Iden "a") "+" (Iden "b"))
>      -- todo: check this is the correct associativity
>     ,("a + b + c", BinOp (Iden "a") "+" (BinOp (Iden "b") "+" (Iden "c")))
>     ,(" a #| multiline\n\
>       \comment |# + b", BinOp (Iden "a") "+" (Iden "b"))

>     ,("true", Iden "true")
>     ,("false", Iden "false")
>     ,("3", num 3)
>     ,("3.3", num 3.3)
>     ,("\"String\"", Sel $ Str "String")
>     --,("\"St\\\"ri\\\"ng\"", Str "St\"ri\"ng")
>     --,("'Str\"ing'", Str "Str\"ing")
>     --,("```multiline\nstring```", Str "multiline\nstring")

>     ,("a(3)", App (Iden "a") [num 3])

>     ,("a(3,4)", App (Iden "a") [num 3, num 4])

>     ,("a and b", BinOp (Iden "a") "and" (Iden "b"))
>     ,("a or b", BinOp (Iden "a") "or" (Iden "b"))
>     ,("(1 < 2)", Parens $ (BinOp (num 1) "<" (num 2)))
>     ,("- a", UnaryMinus (Iden "a"))

>     ,("(a)", Parens (Iden "a"))

>     ,("lam(x): x + 1 end", Lam ["x"] (BinOp (Iden "x") "+" (num 1)))

>     ,("lam(x, y): x - y end"
>      ,Lam ["x","y"] (BinOp (Iden "x") "-" (Iden "y")))

>     ,("lam(x, y): x - y end(1,2)"
>      ,App (Lam ["x","y"] (BinOp (Iden "x") "-" (Iden "y")))
>       [num 1, num 2])

todo: review all the whitespace rules that are being ignored

>     ,("let x=3,y=4: x + y end"
>      , Let [Binding (IdenP NoShadow "x") (num 3)
>            ,Binding (IdenP NoShadow "y") (num 4)]
>          (BinOp (Iden "x") "+" (Iden "y")))
>     ,("let x=3: x + 4 end"
>      ,Let [Binding (IdenP NoShadow "x") (num 3)]
>          (BinOp (Iden "x") "+" (num 4)))

>     ,("let shadow x = 3: x end"
>      ,Let [Binding (IdenP Shadow "x") (num 3)] (Iden "x"))

>     ,("let shadow a = 5, shadow b = 6: a end"
>      ,Let [Binding (IdenP Shadow "a") (num 5)
>           ,Binding (IdenP Shadow "b") (num 6)] (Iden "a"))

>     ,("letrec shadow a = 5: a end"
>      ,LetRec [Binding (IdenP Shadow "a") (num 5)] (Iden "a"))




>     ,("f()", App (Iden "f") [])


>     ,("let x = f(): x end"
>      ,Let [Binding (IdenP NoShadow "x") (App (Iden "f") [])]
>          (Iden "x"))

>     ,("block:\n\
>       \  fun f(a): a + 1 end\n\
>       \end"
>      ,Block[FunDecl "f" ["a"] (BinOp (Iden "a") "+" (num 1)) Nothing])


>     ,("block:\n\
>       \fun f(a):\n\
>       \  a = 1\n\
>       \  a + 1\n\
>       \end\n\
>       \end", Block[FunDecl "f" ["a"] (Block [LetDecl (Binding (IdenP NoShadow "a") (num 1))
>                                             ,StExpr $ BinOp (Iden "a") "+" (num 1)]) Nothing])


>     ,("if a: b end", If [(Iden "a",Iden "b")] Nothing)
>     ,("if a: b else: c end", If [(Iden "a",Iden "b")] (Just (Iden "c")))
>     ,("if a: b else if c: d else: e end"
>      ,If [(Iden "a",Iden "b"),(Iden "c",Iden "d")] (Just (Iden "e")))

>     ,("ask: | otherwise: a end", Ask [] (Just (Iden "a")))


>     ,("ask:\n\
>       \   | a == b then: c\n\
>       \   | c == d then: e\n\
>       \   | otherwise: f\n\
>       \end"
>       ,Ask [(BinOp (Iden "a") "==" (Iden "b"), Iden "c")
>            ,(BinOp (Iden "c") "==" (Iden "d"), Iden "e")
>            ] (Just (Iden "f")))

>     ,("ask:\n\
>       \   | a == b then: c\n\
>       \   | c == d then: e\n\
>       \end"
>       ,Ask [(BinOp (Iden "a") "==" (Iden "b"), Iden "c")
>            ,(BinOp (Iden "c") "==" (Iden "d"), Iden "e")
>            ] Nothing)

>     ,("block: a + 3 end"
>      ,Block [StExpr (BinOp (Iden "a") "+" (num 3))])

>     ,("block:\n\
>       \a = 5\n\
>       \a + 3 end"
>      ,Block [LetDecl (Binding (IdenP NoShadow "a") (num 5.0))
>             ,StExpr (BinOp (Iden "a") "+" (num 3))])

>     ,("lam() : 1 end", Lam [] (num 1))


>     ,("block:\n\
>       \  rec fact = lam(x):\n\
>       \    if x == 0: 1\n\
>       \    else: x * fact(x - 1)\n\
>       \    end\n\
>       \  end\n\
>       \  fact(5)\n\
>       \end"

>      ,Block [RecDecl (Binding (IdenP NoShadow "fact")
>             $ Lam ["x"] $
>                     If [(BinOp (Iden "x") "==" (num 0), num 1)]
>                     (Just (BinOp (Iden "x") "*" (App (Iden "fact") [BinOp (Iden "x") "-" (num 1)]))))
>             ,StExpr (App (Iden "fact") [num 5])])



>     ,("[list: 1,2,3]", Construct (Iden "list") [num 1, num 2, num 3])
>     ,("[list: ]", Construct (Iden "list") [])

>     --,("let {x; y}: {1; 2} in x + y end"
>     -- ,Let [(TupleP [IdenP "x",IdenP "y"], Tuple [num 1, num 2])] (BinOp (Iden "x" "+" (Iden "y"))))

>     -- needs some work in the parser to ignore the trailing ;
>     -- ,("{1; 2;}", Sel $ Tuple [num 1, num 2])
>     ,("{1; 2}", Sel $ Tuple [num 1, num 2])

>     ,("{a: \"one\", b : 2, c : x }"
>      ,Sel $ Record [("a", Sel $ Str "one")
>                    ,("b", num 2)
>                    ,("c", Iden "x")])

>     ,("[list: {\"a\"; 1}, {\"b\"; 2}, {\"c\"; 3}]"
>      ,Construct (Iden "list") [Sel $ Tuple [Sel $ Str "a",num 1]
>                               ,Sel $ Tuple [Sel $ Str "b",num 2]
>                               ,Sel $ Tuple [Sel $ Str "c",num 3]])
>     ,("t.{1}", TupleGet (Iden "t") 1)

>     ,("a.x", DotExpr (Iden "a") "x")

>     ,("cases(List) [list: 1,2,3]:\n\
>       \  | empty => \"empty\"\n\
>       \  | link(f, r) => \"link\"\n\
>       \end"
>      ,Cases "List" (Construct (Iden "list") [num 1, num 2, num 3])
>         [(IdenP NoShadow "empty", Sel $ Str "empty")
>         ,(CtorP "link" [IdenP NoShadow "f", IdenP NoShadow "r"], Sel $ Str "link")]
>         Nothing)

>     ,("cases(List) [list: 1,2,3]:\n\
>       \  | empty => \"empty\"\n\
>       \  | else => \"else\"\n\
>       \end"
>      ,Cases "List" (Construct (Iden "list") [num 1, num 2, num 3])
>         [(IdenP NoShadow "empty", Sel $ Str "empty")]
>         (Just $ Sel $ Str "else"))

>     ,("cases(List) [list: {\"a\"; 1}, {\"b\"; 2}, {\"c\"; 3}]:\n\
>       \  | empty => \"empty\"\n\
>       \  | link({x;y}, r) => x\n\
>       \  | else => \"else\"\n\
>       \end"
>      ,Cases "List" (Construct (Iden "list") [Sel $ Tuple [Sel $ Str "a", num 1]
>                                             ,Sel $ Tuple [Sel $ Str "b", num 2]
>                                             ,Sel $ Tuple [Sel $ Str "c", num 3]])
>         [(IdenP NoShadow "empty", Sel $ Str "empty")
>         ,(CtorP "link" [TupleP [IdenP NoShadow "x", IdenP NoShadow "y"], IdenP NoShadow "r"], Iden "x")]
>         (Just $ Sel $ Str "else"))


>       ]
>  where
>      num = Sel . Num

> parseStmtExamples :: [(String, Stmt)]
> parseStmtExamples =
>     [("when x == 3: 4 end"
>      ,When (BinOp (Iden "x") "==" (Sel $ Num 3)) (Sel $ Num 4))
>     ,("var a = 5", VarDecl (Binding (IdenP NoShadow "a") (num 5)))
>     ,("a := 6", SetVar "a" (num 6))
>     ,("{x; y} = {1; 2}"
>      ,LetDecl (Binding (TupleP [IdenP NoShadow "x", IdenP NoShadow "y"])
>       (Sel $ Tuple [num 1, num 2])))

>     ,("{{w; x}; {y; z}} = x"
>      ,LetDecl (Binding
>                (TupleP [TupleP [IdenP NoShadow "w", IdenP NoShadow "x"]
>                        ,TupleP [IdenP NoShadow "y", IdenP NoShadow "z"]])
>                (Iden "x")))
>
>     ,("{w; x} as wx = z"
>      ,LetDecl (Binding (AsP (TupleP [IdenP NoShadow "w", IdenP NoShadow "x"]) "wx")
>       (Iden "z")))
>     ,("data BTree:\n\
>       \  | node(value, left, right)\n\
>       \  | leaf(value)\n\
>       \end", DataDecl "BTree" [VariantDecl "node" ["value", "left", "right"]
>                     ,VariantDecl "leaf" ["value"]] Nothing)

>     ,("data MyEnum:\n\
>       \  | node(left, right)\n\
>       \  | leaf\n\
>       \end", DataDecl "MyEnum" [VariantDecl "node" ["left", "right"]
>                     ,VariantDecl "leaf" []] Nothing)

>     ,("data MyEnum:\n\
>       \  | node(left, right)\n\
>       \  | leaf()\n\
>       \end", DataDecl "MyEnum" [VariantDecl "node" ["left", "right"]
>                     ,VariantDecl "leaf" []] Nothing)

>     ,("data Point:\n\
>       \  | pt(x, y)\n\
>       \end", DataDecl "Point" [VariantDecl "pt" ["x", "y"]] Nothing)

>     ,("data Point: pt(x, y) end"
>      ,DataDecl "Point" [VariantDecl "pt" ["x", "y"]] Nothing)

>     ,("data Point: pt() end"
>      ,DataDecl "Point" [VariantDecl "pt" []] Nothing)

>     ,("data Point: pt end"
>      ,DataDecl "Point" [VariantDecl "pt" []] Nothing)


>     ,("check \"a first block\":\n\
>       \  5 is 5\n\
>       \  4 is 5\n\
>       \end"
>      ,Check (Just "a first block")
>       [StExpr $ BinOp (num 5) "is" (num 5)
>       ,StExpr $ BinOp (num 4) "is" (num 5)
>       ])

>     ,("check:\n\
>       \  6 is 7\n\
>       \end"
>      ,Check Nothing
>       [StExpr $ BinOp (num 6) "is" (num 7)
>       ])

>     ,("check \"all test syntax\":\n\
>       \  expr1 is expr2\n\
>       \  expr1 is-not expr2\n\
>       \  expr1 is%(pred) expr2\n\
>       \  expr1 is-not%(pred) expr2\n\
>       \  expr satisfies pred\n\
>       \  expr violates pred\n\
>       \  expr raises \"exn-string\"\n\
>       \  expr raises-other-than \"exn-string\"\n\
>       \  expr does-not-raise\n\
>       \  expr raises-satisfies pred\n\
>       \  expr raises-violates pred\n\
>       \end"
>      ,Check (Just "all test syntax")
>       [StExpr $ BinOp (Iden "expr1") "is" (Iden "expr2")
>       ,StExpr $ BinOp (Iden "expr1") "is-not" (Iden "expr2")
>       ,TPred (Iden "expr1") "is%" (Iden "pred") (Iden "expr2")
>       ,TPred (Iden "expr1") "is-not%" (Iden "pred") (Iden "expr2")
>       ,StExpr $ BinOp (Iden "expr") "satisfies" (Iden "pred")
>       ,StExpr $ BinOp (Iden "expr") "violates" (Iden "pred")
>       ,StExpr $ BinOp (Iden "expr") "raises" (Sel $ Str "exn-string")
>       ,StExpr $ BinOp (Iden "expr") "raises-other-than" (Sel $ Str "exn-string")
>       ,TPostfixOp (Iden "expr") "does-not-raise"
>       ,StExpr $ BinOp (Iden "expr") "raises-satisfies" (Iden "pred")
>       ,StExpr $ BinOp (Iden "expr") "raises-violates" (Iden "pred")
>       ])

>     ,("data Point:\n\
>       \  | pt(x, y)\n\
>       \where:\n\
>       \  a-pt = pt(1,2)\n\
>       \  is-Point(a-pt) is true\n\
>       \end"
>      ,DataDecl "Point" [VariantDecl "pt" ["x", "y"]]
>       (Just [LetDecl (Binding (IdenP NoShadow "a-pt") (App (Iden "pt") [num 1, num 2]))
>             ,StExpr $ BinOp (App (Iden "is-Point") [Iden "a-pt"]) "is" (Iden "true")]))
>
>     ,("fun double(n):\n\
>       \  n + n\n\
>       \where:\n\
>       \  double(10) is 20\n\
>       \  double(15) is 30\n\
>       \end"
>      ,FunDecl "double" ["n"] (BinOp (Iden "n") "+" (Iden "n"))
>       (Just [StExpr $ BinOp (App (Iden "double") [num 10]) "is" (num 20)
>            ,StExpr $ BinOp (App (Iden "double") [num 15]) "is" (num 30)]))

>     ,("shadow a = 1"
>      ,LetDecl (Binding (IdenP Shadow "a") (num 1)))
>     ,("rec shadow a = 1"
>      ,RecDecl (Binding (IdenP Shadow "a") (num 1)))

>     ,("var shadow a = 1"
>      ,VarDecl (Binding (IdenP Shadow "a") (num 1)))

>     ,("{shadow x12; shadow y12} = x"
>      ,LetDecl (Binding (TupleP [IdenP Shadow "x12", IdenP Shadow "y12"])
>       (Iden "x")))

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
>                     ,("y", "z")])
>       Nothing [] [StExpr $ Sel $ Num 5])

>     ,("provide *\n\
>       \5"
>      ,Program (Just ProvideAll)
>       Nothing [] [StExpr $ Sel $ Num 5])


>     ,("provide-types {\n\
>       \  Foo:: Foo,\n\
>       \  FooBar:: FooBar\n\
>       \}\n\
>       \5"
>      ,Program Nothing
>       (Just $ ProvideTypes [("Foo", "Foo"),("FooBar", "FooBar")]) [] [StExpr $ Sel $ Num 5])

>     ,("provide-types *\n\
>       \5"
>      ,Program Nothing
>       (Just ProvideTypesAll) [] [StExpr $ Sel $ Num 5])

>     ,("provide *\n\
>       \provide-types *\n\
>       \5"
>      ,Program (Just ProvideAll)
>       (Just ProvideTypesAll) [] [StExpr $ Sel $ Num 5])

>     ,("import file(\"./pyret/main2.arr\") as _\n\
>       \5"
>      ,Program Nothing Nothing
>       [Import (ImportSpecial "file" ["./pyret/main2.arr"]) "_"]
>       [StExpr $ Sel $ Num 5])

>     ,("import s-exp as S\n\
>       \5"
>      ,Program Nothing Nothing
>       [Import (ImportName "s-exp") "S"]
>       [StExpr $ Sel $ Num 5])

>     ,("import is-invalid-array-index from error\n\
>       \5"
>      ,Program Nothing Nothing
>       [ImportFrom ["is-invalid-array-index"] (ImportName "error")]
>       [StExpr $ Sel $ Num 5])

>     ,("import \"./tests/test-strings.arr\" as _\n\
>       \5"
>      ,Program Nothing Nothing
>       [Import (ImportString "./tests/test-strings.arr") "_"]
>       [StExpr $ Sel $ Num 5])

>     ,("import a,b from c\n\
>       \5"
>      ,Program Nothing Nothing
>       [ImportFrom ["a", "b"] (ImportName "c")]
>       [StExpr $ Sel $ Num 5])


>     ,("provide *\n\
>       \provide-types *\n\
>       \import file(\"./pyret/main2.arr\") as _\n\
>       \import s-exp as S\n\
>       \5"
>      ,Program (Just ProvideAll) (Just ProvideTypesAll)
>       [Import (ImportSpecial "file" ["./pyret/main2.arr"]) "_"
>       ,Import (ImportName "s-exp") "S"]
>       [StExpr $ Sel $ Num 5])

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

