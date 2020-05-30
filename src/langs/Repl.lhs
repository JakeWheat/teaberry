

> import Control.Monad.Trans
> import System.Console.Haskeline
>
> import Records1

> type Repl a = InputT IO a

> process :: Env -> String -> IO Env
> process en src =
>     case evaluateFull en src of
>         Left e -> do
>                   putStrLn $ "error: " ++ e
>                   pure en
>         Right (v,en',t) -> do
>             case t of
>                 [] -> pure ()
>                 _ -> putStrLn $ renderCheckResults t
>             putStrLn $ torepr' v
>             --putStrLn $ showEnv en'
>             pure en'


> repl :: Env -> Repl ()
> repl en = do
>   minput <- getInputLine "Repl> "
>   case minput of
>     Nothing -> outputStrLn "Goodbye."
>     Just input -> do
>         e <- liftIO $ process en input
>         repl e


> main :: IO ()
> main = do
>   runInputT defaultSettings (repl defaultEnv)



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





