
> module Langs (langTests) where

> import qualified SimpleExpr
> import qualified SimpleExprError
> import qualified SimpleExprReader
> import qualified SimpleExprDesugared
> import qualified SimplestCheck
> import qualified Expr1FFI
> import qualified Expr1If
> import qualified Expr1NWayIf
> import qualified Expr1IfAndOr
> import qualified Expr1TupleHack
> import qualified Statements1
> import qualified Statements1DesugarLetDecl
> import qualified Statements1Seq
> import qualified Statements1WithoutCheck
> import qualified Statements1Check
> import qualified Statements1CheckDesugarRefactor
> import qualified Var1
> import qualified Lists1
> import qualified DataDecl1
> import qualified Cases1
> import qualified CasesPlusVar
> import qualified Ref1
> import qualified Records1
> import qualified Records1Repl
> import qualified Records1Embedded
> import qualified Import1
> import qualified Import2
> import qualified Import3
> import qualified Import2IO
> import qualified Import4
> import qualified Import4Repl
> import qualified DumpDesugared
> import qualified Expr1Letrec
> import qualified Expr1Letrecstar
> import qualified Statements1FunRec1
> import qualified Statements1FunRec2
> import qualified Tuples1
> import qualified Lists2
> import qualified Records2
> import qualified Construct
> import qualified Variants2
> import qualified Expr1Catch
> import qualified DesugarListBetter
> import qualified OpaqueFFIValues
> import qualified Provide1
> import qualified FixWhere
> import qualified RunScriptInterp
> import qualified Anomaly1
> import qualified SimpleBase
> import qualified Curried


> import qualified Test.Tasty as T

> langTests :: T.TestTree
> langTests = T.testGroup "langs" $ 
>        [SimpleExpr.tests
>        ,SimpleExprError.tests
>        ,SimpleExprReader.tests
>        ,SimpleExprDesugared.tests
>        ,SimplestCheck.tests
>        ,Expr1FFI.tests
>        ,Expr1If.tests
>        ,Expr1NWayIf.tests
>        ,Expr1IfAndOr.tests
>        ,Expr1TupleHack.tests
>        ,Statements1.tests
>        ,Statements1DesugarLetDecl.tests
>        ,Statements1Seq.tests
>        ,Statements1WithoutCheck.tests
>        ,Statements1Check.tests
>        ,Statements1CheckDesugarRefactor.tests
>        ,Var1.tests
>        ,Lists1.tests
>        ,DataDecl1.tests
>        ,Cases1.tests
>        ,CasesPlusVar.tests
>        ,Ref1.tests
>        ,Records1.tests
>        ,Records1Repl.tests
>        ,Records1Embedded.tests
>        ,Import1.tests
>        ,Import2.tests
>        ,Import3.tests
>        ,Import2IO.tests
>        ,Import4.tests
>        ,Import4Repl.tests
>        ,DumpDesugared.tests
>        ,Expr1Letrec.tests
>        ,Expr1Letrecstar.tests
>        ,Statements1FunRec1.tests
>        ,Statements1FunRec2.tests
>        ,Tuples1.tests
>        ,Lists2.tests
>        ,Records2.tests
>        ,Construct.tests
>        ,Variants2.tests
>        ,Expr1Catch.tests
>        ,DesugarListBetter.tests
>        ,OpaqueFFIValues.tests
>        ,Provide1.tests
>        ,FixWhere.tests
>        ,RunScriptInterp.tests
>        ,Anomaly1.tests
>        ,SimpleBase.tests
>        ,Curried.tests
>        ]
