
> module DesugarTests (desugarExamples, testDesugar) where
>
> --import qualified Syntax as S
> import Parse
> import InterpreterSyntax
> import Desugar
>
> import Test.Tasty
> import Test.Tasty.HUnit

> desugarExamples :: [(String, Expr)]
> desugarExamples = [("a", Iden "a")
>                 --,("a + b",  BinOp (Iden "a") "+" (Iden "b"))
>                 ,("true", Iden "true")
>                 ,("false", Iden "false")
>                 ,("3", Num 3)
>                 ,("3.3", Num 3.3)
>                 ,("\"String\"", Str "String")
>                ,("a(3)", App (Iden "a") (Num 3))
>                ,("a(3,4)", App (App (Iden "a") (Num 3)) (Num 4))
>                ,("- a", App (App (Iden "*") (Num (-1))) (Iden "a"))
>                ,("(a)", Iden "a")
>                ,("a + b", App (App (Iden "+") (Iden "a")) (Iden "b"))
>                ,("lam(x): x + 1 end", Lam "x" (App (App (Iden "+") (Iden "x")) (Num 1)))
>                ,("lam(x, y): x - y end"
>                 ,Lam "x" (Lam "y" (App (App (Iden "-") (Iden "x")) (Iden "y"))))
>                ,("lam(x, y): x - y end(1,2)"
>                 ,App (App (Lam "x" (Lam "y" (App (App (Iden "-") (Iden "x")) (Iden "y")))) (Num 1)) (Num 2))
>                ,("let x=3,y=4: x + y end"
>                 ,App (Lam "x"
>                       (App (Lam "y"
>                             (App (App (Iden "+") (Iden "x")) (Iden "y")))
>                        (Num 4))) (Num 3))
>                ,("let x=3: x + 4 end"
>                 ,App (Lam "x" (App (App (Iden "+") (Iden "x")) (Num 4))) (Num 3))
>                ,("if a: b end", If (Iden "a") (Iden "b") (App (Iden "raise") (Str "no branches matched")))
>                ,("if a: b else: c end"
>                 ,If (Iden "a") (Iden "b") (Iden "c"))
>                ,("if a: b else if c: d else: e end"
>                 ,If (Iden "a") (Iden "b")
>                     (If (Iden "c") (Iden "d") (Iden "e")))
>                ,("ask:\n\
>                 \   | a then: c\n\
>                 \   | c then: e\n\
>                 \   | otherwise: f\n\
>                 \end"
>                 ,If (Iden "a") (Iden "c") (If (Iden "c") (Iden "e") (Iden "f")))
>                ,("ask:\n\
>                 \   | otherwise: f\n\
>                 \end"
>                 ,Iden "f")
>                ,("ask:\n\
>                 \end"
>                 ,(App (Iden "raise") (Str "no branches matched")))
>
>                ,("letrec f = lam(a): f(a) end:\n\
>                 \  f('stuff')\n\
>                 \end"
>                 ,p2d "let fXXX = lam(f,a): f(f,a) end,\n\
>                      \    f = lam(a): fXXX(fXXX,a) end:\n\
>                      \f('stuff')\n\
>                      \end")



let f' = lam(f,a): f(f,a) end,
    f = lam(a): f'(f',a) end:
  f('stuff')
end"


test two expressions in a block

>                ,("block: a + b\n\
>                  \  c + d\n\
>                  \end"
>                 ,Block [binop (Iden "a") "+" (Iden "b")
>                        ,binop (Iden "c") "+" (Iden "d")])

test a let then a statement

>                ,("block:\n\
>                 \    a = 5\n\
>                 \    a + 5\n\
>                  \end"
>                 --,Block [App (Lam "a" (Block [(binop (Iden "a") "+" (Num 5))) (Num 5)]))
>                 ,Block [App (Lam "a" (Block [App (App (Iden "+") (Iden "a")) (Num 5)])) (Num 5)])


>                 ]
>   where
>       p2d s = either error id (desugar =<< parseStmt "" s)
>       binop a op b = App (App (Iden op) a) b

> testDesugar :: (String,Expr) -> TestTree
> testDesugar (src, ex) = testCase ("desugar " ++ src) $
>     either error (assertEqual "" ex) $
>       desugar =<< parseStmt "" src
