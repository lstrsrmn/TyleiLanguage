module Main (main) where
import Lexer
import Parser
import Eval
import Syntax
import TypeChecker
import Control.Monad.Except


test :: Raw -> IO ()
test raw =
  case runExcept (infer (Context [] [] [] 0) raw) of
    Left a -> putStrLn a
    Right (tm, ty) -> do
      putStrLn "\nEvaluates to:\n"
      print (nbe tm)

main :: IO ()
main = do
  let tokens = alexScanTokens "let a :: Nat = 5 in a"
  let program = parser tokens
  test program

