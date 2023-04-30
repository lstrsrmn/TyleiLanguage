module Main (main) where
import Lexer
import Parser
import Eval
import Syntax
import TypeChecker
import Control.Monad.Except
import Control.Monad.Reader
import System.Environment

instance ExecEnv IO where
  runPrint = putStr
  runReadFile = readFile


test :: Loc Raw -> String -> IO ()
test raw file =
  case runExcept (infer (Context [] [] [] 0) raw) of
    Left a -> putStrLn (a file)
    Right (tm, _) -> do
      let vtm = eval [] [] tm
      _ <- run vtm
      pure ()

runTest :: String -> IO ()
runTest s = let
  modules = "" -- concat [stringModule] --, tileModule]
  file = modules ++ s
  in case runAlex file (runReaderT parser file) of
    Left e -> error e
    Right rs -> test rs file

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

stringModule :: String
stringModule = "let type String :: Type = ind String with | StrNil () | StrCons (Char, String) in "
tileModule :: String
tileModule = "let type Bool = ind Bool with | True () | False () in \
              \ let type Row = ind Row with | Nil () | Cons (Bool, Row) in \
              \ let type Tile = ind Tile with | Nil () | Cons (Row, Tile) in \
              \ let printBool :: (Bool -> IO ()) = fix printBool :: (Bool -> IO ()) \
              \ | True _ -> print (StrCons ('1', StrNil ())) \
              \ | False _ -> print (StrCons ('0', StrNil ())) \
              \ in \
              \ let printRow :: (Row -> IO ()) = fix printRow :: (Row -> IO ()) \
              \ | Cons r -> do {_ :: () <- printBool (fst r); do {_ :: () <- print (StrCons (' ', StrNil ())); printRow (snd r)}} \
              \ | Nil _ -> print (Cons ('\\n', StrNil ())) \
              \ in let printTile :: (Tile -> IO ()) = fix printTile :: (Tile -> IO ()) \
              \ | Cons t -> do {_ :: () <- printRow (fst t); printTile (snd t)} \
              \ | Nil _ -> print (StrCons ('\\n', StrNil ())) in \
              \ let concatRow :: (Row -> (Row -> Row)) = fix concatRow :: (Row -> (Row -> Row)) \
              \ | Cons r -> (\\x. Cons (fst r, (concatRow (snd r)) x)) \
              \ | Nil _ -> (\\x. x) \
              \ in \
              \ let concatTile :: (Tile -> (Tile -> Tile)) = fix concatTile :: (Tile -> (Tile -> Tile)) \
              \ | Cons t -> (\\x. COns (fst t, (concatTile (snd t)) x)) \
              \ | Nil _ -> (\\x. x) \
              \ in \n"
