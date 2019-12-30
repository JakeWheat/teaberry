
> module DesugarTests (desugarExamples, testDesugar) where
>
> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

> import Parse (parseExpr)
> import InterpreterSyntax (Stmt(..), Expr(..), Selector(..))
> import Desugar (desugarExpr)

> desugarExamples :: [(String, Expr)]
> desugarExamples = [("a", Iden "a")
>                 ,("true", Iden "true")
>                 ,("false", Iden "false")
>                 ,("3", num 3)
>                 ,("3.3", num 3.3)
>                 ,("\"String\"", Sel $ Str "String")
>                ,("a(3)", App (Iden "a") (num 3))
>                ,("a(3,4)", App (App (Iden "a") (num 3)) (num 4))
>                ,("- a", App (App (Iden "*") (num (-1))) (Iden "a"))
>                ,("(a)", Iden "a")
>                ,("a + b", App (App (Iden "+") (Iden "a")) (Iden "b"))
>                ,("lam(x): x + 1 end", Lam "x" (App (App (Iden "+") (Iden "x")) (num 1)))
>                ,("lam(x, y): x - y end"
>                 ,Lam "x" (Lam "y" (App (App (Iden "-") (Iden "x")) (Iden "y"))))
>                ,("lam(x, y): x - y end(1,2)"
>                 ,App (App (Lam "x" (Lam "y" (App (App (Iden "-") (Iden "x")) (Iden "y")))) (num 1)) (num 2))
>                ,("let x=3,y=4: x + y end"
>                 ,Let "x" (num 3) (Let "y" (num 4)
>                                   (App (App (Iden "+") (Iden "x")) (Iden "y"))))
>                ,("let x=3: x + 4 end"
>                 ,Let "x" (num 3) (App (App (Iden "+") (Iden "x")) (num 4)))
>                ,("if a: b end", If (Iden "a") (Iden "b") (App (Iden "raise") (Sel $ Str "no branches matched")))
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
>                 ,(App (Iden "raise") (Sel $ Str "no branches matched")))
>
>                ,("letrec f = lam(a): f(a) end:\n\
>                 \  f('stuff')\n\
>                 \end"
>                 ,p2d "let fXXX = lam(f,a): f(f,a) end,\n\
>                      \    f = lam(a): fXXX(fXXX,a) end:\n\
>                      \f('stuff')\n\
>                      \end")
>                ,("block: a + b\n\
>                  \  c + d\n\
>                  \end"
>                 ,Seq (StExpr $ binop (Iden "a") "+" (Iden "b"))
>                        (StExpr $ binop (Iden "c") "+" (Iden "d")))
>                ,("block:\n\
>                 \    a = 5\n\
>                 \    a + 5\n\
>                  \end"
>                 ,Seq (LetDecl "a" (num 5))
>                      (StExpr $ App (App (Iden "+") (Iden "a")) (num 5)))

desugaring a data decl:

data Point:
  | pt(x, y)
end

->

is-Point = lam(x): I.AppHaskell "DataType" [x] == "Point"
pt = lam (x,y): I.AppHaskell "makeVariant" ("Point", "pt", [list: x,y])
is-pt = lam(x): I.AppHaskell "DataType" [x] == "Point" && I.AppHaskell "VariantType" [x] == "pt"


>                 ]
>   where
>       p2d s = either error id (desugarExpr =<< parseExpr "" s)
>       binop a op b = App (App (Iden op) a) b
>       num = Sel . Num

> testDesugar :: (String,Expr) -> T.TestTree
> testDesugar (src, ex) = T.testCase ("desugar " ++ src) $
>     either error (T.assertEqual "" ex) $
>       desugarExpr =<< parseExpr "" src
