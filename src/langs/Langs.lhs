
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
>        ]
