
> module ParseTests (parseExprExamples
>                   ,testParseExpr
>                   ,parseStmtExamples
>                   ,testParseStmt
>                   ,parseModuleExamples
>                   ,testParseModule
>                   ) where
>
> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

> import Syntax (Stmt(..)
>               ,Expr(..)
>               ,VariantDecl(..)
>               ,Pat(..)
>               ,PatName(..)
>               ,Shadow(..)
>               ,Ref(..)
>               ,Module(..)
>               ,PreludeStmt(..)
>               ,ProvideItem(..)
>               ,ImportSource(..)
>               )

> import Parse (parseExpr, parseStmt, parseModule)
> import Pretty (prettyExpr, prettyStmts, prettyModule)

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
>     ,("3", Num 3)
>     ,("3.3", Num 3.3)
>     ,("\"String\"", Text "String")
>     --,("\"St\\\"ri\\\"ng\"", Text "St\"ri\"ng")
>     --,("'Str\"ing'", Text "Str\"ing")
>     --,("```multiline\nstring```", Text "multiline\nstring")

>     ,("a(3)", App (Iden "a") [Num 3])

>     ,("a(3,4)", App (Iden "a") [Num 3, Num 4])

>     ,("a and b", BinOp (Iden "a") "and" (Iden "b"))
>     ,("a or b", BinOp (Iden "a") "or" (Iden "b"))
>     ,("(1 < 2)", Parens $ (BinOp (Num 1) "<" (Num 2)))
>     ,("- a", UnaryMinus (Iden "a"))

>     ,("e1 ^ e2", BinOp (Iden "e1") "^" (Iden "e2"))
>     --,("e1 ^ e2 ^ e3"
>     -- ,let o a b = BinOp a "^" b
>     --     in (Iden "e1" `o` Iden "e2") `o` Iden "e3")

>     ,("e1 |> e2", BinOp (Iden "e1") "|>" (Iden "e2"))
>     ,("e1 |> e2 |> e3"
>      ,let o a b = BinOp a "|>" b
>       in Iden "e1" `o` (Iden "e2" `o` Iden "e3"))


>     ,("(a)", Parens (Iden "a"))

>     ,("lam(x): x + 1 end", Lam [idenPat "x"] (BinOp (Iden "x") "+" (Num 1)))

>     ,("lam(x, y): x - y end"
>      ,Lam [idenPat "x",idenPat "y"] (BinOp (Iden "x") "-" (Iden "y")))

>     ,("lam(x, y): x - y end(1,2)"
>      ,App (Lam [idenPat "x",idenPat "y"] (BinOp (Iden "x") "-" (Iden "y")))
>       [Num 1, Num 2])

todo: review all the whitespace rules that are being ignored

>     ,("let x=3,y=4: x + y end"
>      , Let [(idenPat "x", Num 3)
>            ,(idenPat "y", Num 4)]
>          (BinOp (Iden "x") "+" (Iden "y")))
>     ,("let x=3: x + 4 end"
>      ,Let [(idenPat "x", Num 3)]
>          (BinOp (Iden "x") "+" (Num 4)))

>     ,("let shadow x = 3: x end"
>      ,Let [(idenPatShadow "x", Num 3)] (Iden "x"))

>     ,("let shadow a = 5, shadow b = 6: a end"
>      ,Let [(idenPatShadow "a", Num 5)
>           ,(idenPatShadow "b", Num 6)] (Iden "a"))

>     ,("letrec shadow a = 5: a end"
>      ,LetRec [(idenPatShadow "a",Num 5)] (Iden "a"))

>     ,("a.b.c"
>      ,DotExpr (DotExpr (Iden "a") "b") "c")

>     ,("f(x)(y)"
>      ,App (App (Iden "f") [Iden "x"])[Iden "y"])


>     ,("ex1!x"
>      ,UnboxRef (Iden "ex1") "x")
>     ,("ex2!x!y"
>      ,UnboxRef (UnboxRef (Iden "ex2") "x") "y")
>     ,("ex2.y!x"
>      ,UnboxRef (DotExpr (Iden "ex2") "y") "x")
> 
>     ,("f()", App (Iden "f") [])


>     ,("let x = f(): x end"
>      ,Let [(idenPat "x", App (Iden "f") [])]
>          (Iden "x"))

>     ,("block:\n\
>       \  fun f(a): a + 1 end\n\
>       \end"
>      ,Block[FunDecl (PatName NoShadow "f") [idenPat "a"] (BinOp (Iden "a") "+" (Num 1)) Nothing])

>     ,("block:\n\
>       \  fun shadow f(a): a + 1 end\n\
>       \end"
>      ,Block[FunDecl (PatName Shadow "f") [idenPat "a"] (BinOp (Iden "a") "+" (Num 1)) Nothing])



>     ,("block:\n\
>       \fun f(a):\n\
>       \  a = 1\n\
>       \  a + 1\n\
>       \end\n\
>       \end", Block[FunDecl (PatName NoShadow "f") [idenPat "a"] (Block [LetDecl (idenPat "a") (Num 1)
>                                              ,StExpr $ BinOp (Iden "a") "+" (Num 1)]) Nothing])


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
>      ,Block [StExpr (BinOp (Iden "a") "+" (Num 3))])

>     ,("block:\n\
>       \a = 5\n\
>       \a + 3 end"
>      ,Block [LetDecl (idenPat "a") (Num 5.0)
>             ,StExpr (BinOp (Iden "a") "+" (Num 3))])

>     ,("lam() : 1 end", Lam [] (Num 1))


>     ,("block:\n\
>       \  rec fact = lam(x):\n\
>       \    if x == 0: 1\n\
>       \    else: x * fact(x - 1)\n\
>       \    end\n\
>       \  end\n\
>       \  fact(5)\n\
>       \end"

>      ,Block [RecDecl (idenPat "fact")
>             $ Lam [idenPat "x"] $
>                     If [(BinOp (Iden "x") "==" (Num 0), Num 1)]
>                     (Just (BinOp (Iden "x") "*" (App (Iden "fact") [BinOp (Iden "x") "-" (Num 1)])))
>             ,StExpr (App (Iden "fact") [Num 5])])



>     ,("[list: 1,2,3]", Construct (Iden "list") [Num 1, Num 2, Num 3])
>     ,("[list: ]", Construct (Iden "list") [])
>     -- todo: make a commaSepTrailing or something
>     --,("[list: 1,2,]", Construct (Iden "list") [Num 1, Num 2])

>     --,("let {x; y}: {1; 2} in x + y end"
>     -- ,Let [(TupleP [IdenP "x",IdenP "y"], Tuple [Num 1, Num 2])] (BinOp (Iden "x" "+" (Iden "y"))))

>     ,("{1; 2;}", TupleSel [Num 1, Num 2])
>     ,("{1; 2}", TupleSel [Num 1, Num 2])

>     ,("{a; b}", TupleSel [Iden "a", Iden "b"])

>     ,("{a: \"one\", b : 2, c : x }"
>      ,RecordSel [("a", Text "one")
>                    ,("b", Num 2)
>                    ,("c", Iden "x")])
>     ,("{a: 1,}"
>      ,RecordSel [("a", Num 1)])


>     ,("[list: {\"a\"; 1}, {\"b\"; 2}, {\"c\"; 3}]"
>      ,Construct (Iden "list") [TupleSel [Text "a",Num 1]
>                               ,TupleSel [Text "b",Num 2]
>                               ,TupleSel [Text "c",Num 3]])
>     ,("t.{1}", TupleGet (Iden "t") 1)

>     ,("a.x", DotExpr (Iden "a") "x")

>     ,("cases(List) [list: 1,2,3]:\n\
>       \  | empty => \"empty\"\n\
>       \  | link(f, r) => \"link\"\n\
>       \end"
>      ,Cases "List" (Construct (Iden "list") [Num 1, Num 2, Num 3])
>         [(IdenP $ idenPat "empty", Text "empty")
>         ,(VariantP Nothing "link" [IdenP $ idenPat "f", IdenP $ idenPat "r"], Text "link")]
>         Nothing)

>     ,("cases(List) [list: 1,2,3]:\n\
>       \  | empty => \"empty\"\n\
>       \  | else => \"else\"\n\
>       \end"
>      ,Cases "List" (Construct (Iden "list") [Num 1, Num 2, Num 3])
>         [(IdenP $ idenPat "empty", Text "empty")]
>         (Just $ Text "else"))

todo:
cases (X) x:
  | empty => a
  | empty as my_empty => a
  | _ as my_empty => a
  | link(a,empty) as x => a
  | link(a,b) => a
  | link(a,b) as x => a
  | link(c,link(d,empty)) => a
  | link(c link(d,empty) as x) => a
end


>     {-,("cases(List) [list: {\"a\"; 1}, {\"b\"; 2}, {\"c\"; 3}]:\n\
>       \  | empty => \"empty\"\n\
>       \  | link({x;y}, r) => x\n\
>       \  | else => \"else\"\n\
>       \end"
>      ,Cases "List" (Construct (Iden "list") [TupleSel [Text "a", Num 1]
>                                             ,TupleSel [Text "b", Num 2]
>                                             ,TupleSel [Text "c", Num 3]])
>         [(idenPat "empty", Text "empty")
>         ,(VariantP Nothing "link" [TupleP [idenPat "x", idenPat "y"], idenPat "r"], Iden "x")]
>         (Just $ Text "else"))-}

>     ,("cases(z.List) [list: {\"a\"; 1}, {\"b\"; 2}, {\"c\"; 3}]:\n\
>       \  | z.empty => \"empty\"\n\
>       \  | z.link(f, r) => x\n\
>       \  | else => \"else\"\n\
>       \end"
>      ,Cases "z.List" (Construct (Iden "list") [TupleSel [Text "a", Num 1]
>                                               ,TupleSel [Text "b", Num 2]
>                                               ,TupleSel [Text "c", Num 3]])
>         [(VariantP (Just "z") "empty" [], Text "empty")
>         ,(VariantP (Just "z") "link" [IdenP $ idenPat "f", IdenP $ idenPat "r"], Iden "x")]
>         (Just $ Text "else"))


>   {-  ,("cases(z.List) [list: {\"a\"; 1}, {\"b\"; 2}, {\"c\"; 3}]:\n\
>       \  | z.empty => \"empty\"\n\
>       \  | z.link({x;y}, r) => x\n\
>       \  | else => \"else\"\n\
>       \end"
>      ,Cases "z.List" (Construct (Iden "list") [Sel $ TupleSel [Sel $ Text "a", num 1]
>                                               ,Sel $ TupleSel [Sel $ Text "b", num 2]
>                                               ,Sel $ TupleSel [Sel $ Text "c", num 3]])
>         [(VariantP (Just "z") "empty" [], Sel $ Text "empty")
>         ,(VariantP (Just "z") "link" [TupleP [idenPat "x", idenPat "y"], idenPat "r"], Iden "x")]
>         (Just $ Sel $ Text "else"))-}


>       ]
>  where
>    idenPat x = PatName NoShadow x
>    idenPatShadow x = PatName Shadow x

> parseStmtExamples :: [(String, Stmt)]
> parseStmtExamples =
>     [("when x == 3: 4 end"
>      ,When (BinOp (Iden "x") "==" (Num 3)) (Num 4))
>     ,("var a = 5", VarDecl (idenPat "a") (Num 5))
>     ,("a := 6", SetVar "a" (Num 6))
>     {-,("{x; y} = {1; 2}"
>      ,LetDecl (TupleP [idenPat "x", idenPat "y"])
>       (TupleSel [Num 1, Num 2]))

>     ,("{{w; x}; {y; z}} = x154"
>      ,LetDecl (TupleP [TupleP [idenPat "w", idenPat "x"]
>                        ,TupleP [idenPat "y", idenPat "z"]])
>                (Iden "x154"))
>
>     ,("{w; x} as wx = z"
>      ,LetDecl (AsP (TupleP [idenPat "w", idenPat "x"]) (PatName NoShadow "wx"))
>       (Iden "z"))

>     ,("{w; x} as shadow wx = z"
>      ,LetDecl (AsP (TupleP [idenPat "w", idenPat "x"]) (PatName Shadow "wx"))
>       (Iden "z"))-}



>     ,("data BTree:\n\
>       \  | node(value, left, right)\n\
>       \  | leaf(value)\n\
>       \end", DataDecl "BTree" [VariantDecl "node" [(Con, "value"), (Con, "left"), (Con, "right")]
>                     ,VariantDecl "leaf" [(Con, "value")]] Nothing)

>     ,("data MyEnum:\n\
>       \  | node(left, right)\n\
>       \  | leaf\n\
>       \end", DataDecl "MyEnum" [VariantDecl "node" [(Con, "left"), (Con, "right")]
>                     ,VariantDecl "leaf" []] Nothing)

>     ,("data MyEnum:\n\
>       \  | node(left, right)\n\
>       \  | leaf()\n\
>       \end", DataDecl "MyEnum" [VariantDecl "node" [(Con, "left"), (Con, "right")]
>                     ,VariantDecl "leaf" []] Nothing)

>     ,("data Point:\n\
>       \  | pt(x, y)\n\
>       \end", DataDecl "Point" [VariantDecl "pt" [(Con, "x"), (Con, "y")]] Nothing)

>     ,("data Point: pt(x, y) end"
>      ,DataDecl "Point" [VariantDecl "pt" [(Con, "x"), (Con, "y")]] Nothing)

>     ,("data Point: pt() end"
>      ,DataDecl "Point" [VariantDecl "pt" []] Nothing)

>     ,("data Point: pt end"
>      ,DataDecl "Point" [VariantDecl "pt" []] Nothing)


>     ,("data MutX:\n\
>       \  | mut-x(ref x, y)\n\
>       \end"
>       ,DataDecl "MutX" [VariantDecl "mut-x" [(Ref,"x"), (Con,"y")]] Nothing)
>     ,("ex1!{x: 42}"
>      ,SetRef (Iden "ex1") [("x", Num 42)])
>     ,("ex1!{x: 42, y:43}"
>      ,SetRef (Iden "ex1") [("x", Num 42), ("y", Num 43)])
>     ,("ex1!a!{x: 42}"
>      ,SetRef (UnboxRef (Iden "ex1") "a") [("x", Num 42)])

>     ,("ex1.a!{x: 42}"
>      ,SetRef (DotExpr (Iden "ex1") "a") [("x", Num 42)])



check:
  ex1!x is 1      # this access the value inside the reference
  ex1.x is-not 1  # this does not
end

ex1!{x: 42}
check:
  ex1!x is 42
end



>     ,("check \"a first block\":\n\
>       \  5 is 5\n\
>       \  4 is 5\n\
>       \end"
>      ,Check (Just "a first block")
>       [StExpr $ BinOp (Num 5) "is" (Num 5)
>       ,StExpr $ BinOp (Num 4) "is" (Num 5)
>       ])

>     ,("check:\n\
>       \  6 is 7\n\
>       \end"
>      ,Check Nothing
>       [StExpr $ BinOp (Num 6) "is" (Num 7)
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
>       ,StExpr $ BinOp (Iden "expr") "raises" (Text "exn-string")
>       ,StExpr $ BinOp (Iden "expr") "raises-other-than" (Text "exn-string")
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
>      ,DataDecl "Point" [VariantDecl "pt" [(Con, "x"), (Con, "y")]]
>       (Just [LetDecl (idenPat "a-pt") (App (Iden "pt") [Num 1, Num 2])
>             ,StExpr $ BinOp (App (Iden "is-Point") [Iden "a-pt"]) "is" (Iden "true")]))
>
>     ,("fun double(n):\n\
>       \  n + n\n\
>       \where:\n\
>       \  double(10) is 20\n\
>       \  double(15) is 30\n\
>       \end"
>      ,FunDecl (PatName NoShadow "double") [idenPat "n"] (BinOp (Iden "n") "+" (Iden "n"))
>       (Just [StExpr $ BinOp (App (Iden "double") [Num 10]) "is" (Num 20)
>            ,StExpr $ BinOp (App (Iden "double") [Num 15]) "is" (Num 30)]))

>     ,("shadow a = 1"
>      ,LetDecl (idenPatShadow "a") (Num 1))
>     ,("rec shadow a = 1"
>      ,RecDecl (idenPatShadow "a") (Num 1))

>     ,("var shadow a = 1"
>      ,VarDecl (idenPatShadow "a") (Num 1))

>     {-,("{shadow x12; shadow y12} = x"
>      ,LetDecl (TupleP [idenPatShadow "x12", idenPatShadow "y12"])
>       (Iden "x"))-}

>     ]
>  where
>    idenPat x = PatName NoShadow x
>    idenPatShadow x = PatName Shadow x

> parseModuleExamples :: [(String,Module)]
> parseModuleExamples =
>     [("\"hello\""
>      ,Module [] [StExpr $ Text "hello"])

>     ,("provide: * end\n\
>       \1"
>      ,Module [Provide [ProvideAll]] [StExpr $ Num 1])


>     ,("provide: a end\n\
>       \1"
>      ,Module [Provide [ProvideName "a"]] [StExpr $ Num 1])

>     ,("provide: a,b end\n\
>       \1"
>      ,Module [Provide [ProvideName "a", ProvideName "b"]] [StExpr $ Num 1])


>     ,("provide: a as b end\n\
>       \1"
>      ,Module [Provide [ProvideAlias "a" "b"]] [StExpr $ Num 1])



>     ,("include file(\"file.tea\")\n\
>       \1"
>      ,Module [Include (ImportSpecial "file" ["file.tea"])] [StExpr $ Num 1])

>     ,("include string-dict\n\
>       \1"
>      ,Module [Include (ImportName "string-dict")] [StExpr $ Num 1])

>     ,("import file(\"file.tea\") as X\n\
>       \1"
>      ,Module [Import (ImportSpecial "file" ["file.tea"]) "X"] [StExpr $ Num 1])

>     ,("import string-dict as X\n\
>       \1"
>      ,Module [Import (ImportName "string-dict") "X"] [StExpr $ Num 1])


>     ,("include from X: * end\n\
>       \1"
>      ,Module [IncludeFrom "X" [ProvideAll]] [StExpr $ Num 1])

>     ,("include from X: a end\n\
>       \1"
>      ,Module [IncludeFrom "X" [ProvideName "a"]] [StExpr $ Num 1])

>     ,("include from X: a,b end\n\
>       \1"
>      ,Module [IncludeFrom "X" [ProvideName "a", ProvideName "b"]] [StExpr $ Num 1])

>     ,("include from X: a as b end\n\
>       \1"
>      ,Module [IncludeFrom "X" [ProvideAlias "a" "b"]] [StExpr $ Num 1])


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

> testParseModule :: (String,Module) -> T.TestTree
> testParseModule t = testParseX parseModule prettyModule t

