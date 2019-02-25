
> module InterpreterTests (interpreterExamples, testInterpreter) where
>
> --import qualified Syntax as S
> import Parse
> import Desugar
> import Interpreter
> import qualified InterpreterSyntax as I
>
> import Test.Tasty
> import Test.Tasty.HUnit

> interpreterExamples :: [(String, Env, Value)]
> interpreterExamples = [("2", emptyEnv, NumV 2)
>                       ,("myVar", simpleEnv, NumV 1)]
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
> s2p2d s = desugar =<< parseExpr "" s

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
> testInterpreter (src, env, ex) = testCase ("parse " ++ src) $
>     either error (assertEqual "" ex) $
>        interp env =<< desugar =<< parseExpr "" src
