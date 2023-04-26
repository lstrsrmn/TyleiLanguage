module Main (main) where
import Lexer
import Parser
import Eval
import Syntax
import TypeChecker
import Control.Monad.Except
import System.IO
import System.Environment

instance ExecEnv IO where
  runPrint = putStrLn
  runReadFile = readFile

stringModule :: String
stringModule = "let type String = ind String with | StrNil () | StrCons (Char, String) in "

test :: Raw -> IO ()
test raw =
  case runExcept (infer (Context [] [] [] 0) raw) of
    Left a -> putStrLn a
    Right (tm, ty) -> do
      let vtm = eval [] [] tm
      putStrLn "\nEvaluates to:\n"
      print (quote 0 vtm)
      putStrLn "\nRunning Program:\n"
      utm <- run vtm
      putStrLn "\nFinalResult\n"
      print (quote 0 utm)

runTest :: String -> IO ()
runTest = test . parser . alexScanTokens . (stringModule++)

runFromFile :: String -> IO ()
runFromFile s = do
  a <- readFile s
  runTest a
  
main :: IO ()
main = do
  (arg:_) <- getArgs
  putStrLn "Running:\n"
  putStrLn arg
  runFromFile arg

