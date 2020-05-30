
TODO:

what would have the biggest improvement on making this more usable:
import, loading files
multiline editing
hooked up to the main language

watch mode - watch a file and rerun it when it changes
  display errors or the output, test cases
only showing failed tests

script mode: support command line args for an in language script



once the file loader is implemented, implement import and include in
the repl

find a way to separate the original env from stuff added in the repl to show it better
+ format it better

command to dump the current env to a file as source
option to include shadowed stuff? will this be garbage collected if
it's no longer reachable? assume this, which means there's only one
version, which dumps everything needed
how to dump shared envs once lost the fact they are shared?
how to dump shared store entries/ pickling?
don't solve these immediately, put them on a list

work on command line opts and commands

add all the languages



make sure it's transactional for errors: it resets to the env before
 the statement with the error

add all the other little features

support ^C better
  I think this needs threads/fork or something to be able to interrupt
   something running
  but can support ^C to reset the current input text

see how can support multiline stuff

write some tests for the process function

replace the main command exe

> import Control.Monad.Trans
> import System.Console.Haskeline
>
> import Records1Repl (ReplImplHandle
>                     ,startReplImpl
>                     ,evaluateLine)

> type Repl a = InputT IO a

> process :: ReplImplHandle -> String -> IO ()
> process h src = do
>     x <- evaluateLine h src
>     case x of
>         Left y -> putStrLn $ "error: " ++ y
>         Right Nothing -> pure ()
>         Right (Just s) -> putStrLn s


> repl :: ReplImplHandle -> Repl ()
> repl h = go
>   where
>     go = do
>         minput <- getInputLine "t > "
>         case minput of
>             Nothing -> pure ()
>             Just input -> do
>                 liftIO $ process h input
>                 go

> main :: IO ()
> main = do
>     h <- startReplImpl
>     runInputT st (repl h)
>   where
>     st = defaultSettings {historyFile = (Just ".teaberryreplhistory")}


how does a repl work with the system?
1. a command to show the current bindings (env)
you pass the env to evaluate
it returns the value of what you entered
it saves the env
you don't keep a compiler open, you just preserve the env each time
how do you load and run tests for a module?

reload command
load command
should it track changes and reload automatically?
load is "import"
reload is "import"
ability to say run tests for loaded modules or don't run them
front end:
  pass an env, and some source
  it runs it, and returns the env, the value, and the test results

other command line options:
run script in file(s)
run script given in command line arg
choose which little language to use
option to run the tests for that language
dump the ffi catalog for that language

-> how to make this work? do a classic "oo-in-c" approach
create a data type with all the things you need to support a language version
then there's a list of these, one per little language
hook up the existing implementation to all this too

1 design the command line options
implement them on top of a single version of the language?
2 design the api between the command line and a little language
3 add the little languages and start using it

see if can figure out how to run tasty with args from this command
line - something to pass args after -- to tasty?

-f
-c
can repeat these
without either, it's repl
--repl add repl even if -c -f are added, run them first
--run-tests
  run the tests for the scripts passed
--language choose which language you want
  you can pass all for some options
--internal-tests run the tasty tests, also uses --language to restrict,
  leave it off to run all tests
--help
--compiler-report?
--format
--show-ast
--show-desugared
--show-desugared-ast
--dump-ffi
future:
pass args to script

parserize?

repl commands:
load file - use import
edit line in external editor - leave for now?

^c to return to prompt
^d to exit
idea: something to enter multiline mode, and to exit
in multiline mode you do ctrl-d to send a command
and return inserts a nwwline
not sure about ctrl-d, what about ctrl-enter





