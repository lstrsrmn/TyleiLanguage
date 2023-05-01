{-# LANGUAGE  QuasiQuotes #-}
module Main (main) where
import Lexer
import Parser
import Eval
import Syntax
import TypeChecker
import Control.Monad.Except
import Control.Monad.Reader
import System.Environment
import Text.RawString.QQ

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
  refactor '\n' = ' '
  refactor c = c
  file = (map refactor tileModule) ++ "\n" ++ s
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

tileModule :: String
tileModule = [r|
let type String :: Type = ind String with | Nil () | Cons (Char, String) in
let type List :: Type -> Type = \A. ind List with | Nil () | Cons (A, List) in
let type Bool :: Type = ind Bool with | True () | False () in
let type Row :: Type = List Bool in
let type Tile :: Type = List Row in
let printBool :: (Bool -> IO ()) = fix printBool :: (Bool -> IO ())
| True _ -> print (Cons ('1', Nil ()))
| False _ -> print (Cons ('0', Nil ()))
in
let printRow :: (Row -> IO ()) = fix printRow :: (Row -> IO ())
| Cons r -> do {_ :: () <- printBool (fst r); printRow (snd r)}
| Nil _ -> print (Cons ('\n', Nil ()))
in let printTile :: (Tile -> IO ()) = fix printTile :: (Tile -> IO ())
| Cons t -> do {_ :: () <- printRow (fst t); printTile (snd t)}
| Nil _ -> print (Cons ('\n', Nil ()))
in
let concat :: forall A :: Type . (List A -> (List A -> List A)) =
/\A. fix concat :: (List A -> (List A -> List A))
| Cons c -> (\x. Cons (fst c, (concat (snd c)) x))
| Nil _ -> (\x .x)
in
let map :: forall A :: Type . forall B :: Type . ((A -> B) -> List A -> List B) =
/\A. /\B. \f. fix map :: List A -> List B
| Cons c -> Cons (f (fst c), map (snd c))
| Nil _ -> Nil ()
in
let head :: forall A :: Type . List A -> A =
/\A. fix head :: List A -> A
| Cons c -> fst c
in
let tail :: forall A :: Type . List A -> List A =
/\A. fix tail :: List A -> List A
| Cons c -> snd c
in
let not :: Bool -> Bool =
fix _ :: Bool -> Bool
| True _ -> False ()
| False _ -> True ()
in
let length :: forall A :: Type . List A -> Nat =
/\A. fix length :: List A -> Nat
| Cons a -> 1 + (length (snd a))
| Nil _ -> 0
in

let first :: forall A ::Type . forall B :: Type . forall C :: Type . (A -> B) -> (A, C) -> (B, C) =
/\A. /\B. /\C. \f. \pair. (f (fst pair), snd pair)
in

let if :: forall A :: Type . Bool -> A -> A -> A =
    /\A. \b. \t. \f.
    let f :: Bool -> A =
        fix _ :: Bool -> A
        | True _ -> t
        | False _ -> f
    in f b
in
let isEmpty :: forall A :: Type . List A -> Bool =
/\A. fix _ :: List A -> Bool
     | Cons a -> False ()
     | Nil _ -> True ()
in
let emptyLeft :: forall A :: Type . forall B :: Type . (List A, List B) -> Bool =
/\A. /\B. \x. isEmpty @A (fst x)
in
let emptyRight :: forall A :: Type . forall B :: Type . (List A, List B) -> Bool =
/\A. /\B. \x. isEmpty @B (snd x)
in
let parseFile :: String -> Tile =
\inp.
    let parseChar :: (Char -> Row -> Row) = \c. matchChar :: (Row -> Row) c with
    | '0' -> (\x. Cons( False (), x))
    | '1' -> (\x. Cons( True (), x))
    | '\n' -> (\_ . Nil ())
    in
    let parseRow :: String -> Row =
    fix parseRow :: String -> Row
    | Cons a -> parseChar (fst a) (parseRow (snd a))
    | Nil _ -> Nil ()
    in
    let split :: String -> (String, String) =
        let isNL :: Char -> Bool =
            \c. matchChar :: Bool c with
            | '\n' -> True ()
            | _ -> False ()
        in
        fix split :: String -> (String, String)
        | Cons a -> if @(String, String) (isNL (fst a)) (Nil (), snd a) (
                       let result :: (String, String) = split (snd a) in
                       (Cons (fst a, fst result), snd result)
                    )
        | Nil _ -> (Nil (), Nil ())
    in
        let splitRows :: String -> List String =
        fix splitRows :: String -> List String
        | Cons a ->
                    let str :: String = Cons (fst a, snd a) in
                    let splt :: (String, String) = split str in
                    if @(List String) (emptyRight @Char @Char splt) (Cons (fst splt, Nil ()))
                          (Cons (fst splt, splitRows (snd splt)))
        | Nil _ -> Nil ()
        in
        map @String @Row parseRow (splitRows inp)
in
let scale :: forall A :: Type . Nat -> List A -> List A =
/\A. \scaleFactor.
     fix scaleUp :: List A -> List A
     | Cons a -> iter :: (List A) ( scaleFactor , (scaleUp (snd a)) , (\x. Cons (fst a, x)) )
     | Nil _ -> Nil ()
in
let transpose :: forall A :: Type . List (List A) -> List (List A) =
    /\A. fix transpose :: List (List A) -> List (List A)
         | Cons a -> if @(List (List A)) (isEmpty @A (fst a)) (Nil ())
                        (let li :: List (List A) = Cons (fst a, snd a) in
                         Cons (map @(List A) @A (head @A) li,
                               transpose (map @(List A) @(List A) (tail @A) li))
                        )
         | Nil _ -> Nil ()
in
let reverse :: forall A :: Type . List A -> List A =
/\A. fix reverse :: List A -> List A
     | Cons a -> concat @A (reverse (snd a)) (Cons (fst a, Nil ()))
     | Nil _ -> Nil ()
in
let rotate90 :: forall A :: Type . List (List A) -> List (List A) =
    /\A. \li. transpose @A (reverse @(List A) li)
in
let reflectV :: forall A :: Type . List (List A) -> List (List A) =
/\A. \li. map @(List A) @(List A) (reverse @A) li
in
let reflectH :: forall A :: Type . List (List A) -> List (List A) =
/\A. \li. transpose @A (map @(List A) @(List A) (reverse @A) (transpose @A li))
in
let add :: forall A :: Type . List (List A) -> List (List A) -> List (List A) =
/\A. \t1. \t2.
     let addRecurse :: List (List A) -> List (List A) -> List (List A) =
         fix addRecurse :: List (List A) -> List (List A) -> List (List A)
             | Cons a -> \x. Cons (concat @A (fst a) (head @(List A) x), addRecurse (snd a) (tail @(List A) x))
             | Nil _ -> \x. x
     in addRecurse t1 t2
in
let and :: Bool -> Bool -> Bool =
    \b1. \b2. if @Bool b1 (if @Bool b2 (True ()) (False ())) (False ())
in
let or :: Bool -> Bool -> Bool =
    \b1. \b2. if @Bool b1 (True ()) (if @Bool b2 (True ()) (False ()))
in
let listOp :: forall A :: Type . (A -> A -> A) -> List A -> List A -> List A =
/\A. \f.
       fix listOp :: List A -> List A -> List A
       | Cons a -> \x. Cons (f (fst a) (head @A x), listOp (snd a) (tail @A x))
       | Nil _ -> \x. x
in
let append :: forall A :: Type . List A -> A -> List A =
    /\A. \li. \t. concat @A li (Cons (t, Nil ()))
in
let subList :: forall A :: Type . Nat -> List A -> List A =
   /\A. \n. \l. snd (iter :: (List A, List A) (n, (l, Nil ()), (\x. (tail @A (fst x), append @A (snd x) (head @A (fst x))))))
in
let subMat :: forall A :: Type . Nat -> Nat -> List (List A) -> List (List A) =
/\A. \n1. \n2. \m. subList @(List A) n2 (map @(List A) @(List A) (subList @A n1) m)
in
|]
