
> module InterpreterTests (interpreterExamples, testInterpreter) where
>
> import Parse
> import Desugar
> import Interpreter
> import qualified InterpreterSyntax as I
>
> import Test.Tasty
> import Test.Tasty.HUnit

> interpreterExamples :: [(String, Env, Value)]
> interpreterExamples =
>     [("2", emptyEnv, NumV 2)
>     ,("myVar", simpleEnv, NumV 1)
>     ,("true", emptyEnv, BoolV True) 
>     ,("false", emptyEnv, BoolV False)
>     ,("\"hello\"", emptyEnv, StrV "hello")
>     ,("1 + 2", simpleEnv, NumV 3)
>     ,("3 - 2", simpleEnv, NumV 1)
>     ,("if true: 1 else: 2 end", simpleEnv, NumV 1)
>     ,("if false: 1 else: 2 end", simpleEnv, NumV 2)
>     ,("add2(1)", simpleEnv, NumV 3)
>     ,("5 + quad(3)", simpleEnv, NumV 17)
>     ,("const5(3) * 4", simpleEnv, NumV 20)
>     ,("10 + const5(10)", simpleEnv, NumV (10 + 5))
>     ,("10 + double(1 + 2)", simpleEnv, NumV (10 + 3 + 3))
>     ,("10 + quad(1 + 2)", simpleEnv, NumV (10 + 3 + 3 + 3 + 3))
>     ,("lam(x): x + 5 end(10)", simpleEnv, NumV 15)
>     ,("let x=4,y=5: x + y end", simpleEnv, NumV 9)
>     ,("let x=4,y=5: x - y end", simpleEnv, NumV (-1))
>     ,("let myF=lam(x): x + 5 end: myF(20) end", simpleEnv, NumV 25)
>     ,("block: 1 + 2\n  3 + 4\nend", simpleEnv, NumV 7)
>
>     ,("letrec fact = lam(n):\n\
>       \    if n == 1: 1 else: n * fact(n - 1) end\n\
>       \end: fact(5) end", simpleEnv, NumV 120)

>     ,("block:\n\
>       \  a = 4\n\
>       \  a + 3\n\
>       \end", simpleEnv, NumV 7)

>     ,("block:\n\
>       \  a = 4\n\
>       \  b = 5\n\
>       \  a + b\n\
>       \end", simpleEnv, NumV 9)


>     --,("let f = lam(x) block:\n
>     -- \
>     ]
>     where
>       simpleEnv =
>          either error id $
>           addFn "add2" "lam (a): a + 2 end"
>           =<< addFn "f4" "lam (x): if x: 1 else: 0 end end"
>           =<< addFn "const5" "lam (_): 5 end"
>           =<< addFn "quad" "lam (x): double (double (x)) end"
>           =<< addFn "double" "lam (x): x + x end"
>           (extendsEnv [("myVar", NumV 1)]
>               defaultHaskellFFIEnv)

> s2p2d :: String -> Either String I.Expr
> s2p2d s = desugar =<< parseStmt "" s

> addFn :: String -> String -> Env -> Either String Env
> addFn nm src env = do
>     ast <- s2p2d src
>     x <- interp env ast
>     case x of
>         ClosV {} -> Right $ extendEnv nm x env
>         _ -> Left $ "expected closure in addFn, got " ++ show x
>         
>         


> testInterpreter :: (String,Env,Value) -> TestTree
> testInterpreter (src, env, ex) = testCase ("interp " ++ src) $
>     either error (assertEqual "" ex) $
>        interp env =<< desugar =<< parseStmt "" src
