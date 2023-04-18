{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Syntax where

type Name = String

data Type v
  = TVar v
  | Unit
  | Function (Type v) (Type v)
  | Nat
  | Product (Type v) (Type v)
  | ForAll Name (Type v)
  | Ind Name [(Name, Type v)]
  | IO (Type v)
  | TVChar
  deriving Show

data Raw
  = RVar Name -- done
  | RStar -- done
  | RAbs Name Raw -- Done
  | RApp Raw Raw -- Done
  | RNum Int -- Done
  | RChar Char
  -- Iter (C, n, t0, ts)
  -- C -> Return type
  -- n -> number of iterations
  -- t0 -> base case
  -- ts -> recursive step
  -- e.g. Iter (_, 4, t0, ts) = ts(ts(ts(ts(t0))))
  | RIter (Type Name) Raw Raw Raw -- Done
  | RPair Raw Raw -- Done
  | RFst Raw -- Done
  | RSnd Raw -- Done
  | RTypeAbs Name Raw -- 
  | RTypeApp Raw (Type Name) -- Done
  | RCons Name Raw -- Done
  | RFix Name (Type Name) [(Name, Name, Raw)] -- Done
  | RBind Name (Type Name) Raw Raw
  | RReturn Raw
  | RLet Name (Type Name) Raw Raw -- let x : Type = e1 in e2
  | RLetType Name (Type Name) Raw -- let type x : Type in e

newtype Ix = Ix Int
  deriving (Eq, Ord, Num, Show)


data Term
  = Var Ix
  | Star
  | Abs Name Term
  | App Term Term
  | Num Int
  | TChar Char
  -- Iter (C, n, t0, ts)
  -- C -> Return type
  -- n -> number of iterations
  -- t0 -> base case
  -- ts -> recursive step
  -- e.g. Iter (_, 4, t0, ts) = ts(ts(ts(ts(t0))))
  | Iter (Type Ix) Term Term Term
  | Pair Term Term
  | Fst Term
  | Snd Term
  | TypeAbs Name Term
  | TypeApp Term (Type Ix)
  | Cons Name Term
  | Bind Name (Type Ix) Term Term
  | Return Term
  | Fix Name (Type Ix) [(Name, Name, Term)]
  | Let Name (Type Ix) Term Term
  | LetType Name (Type Ix) Term

data Context = Context {
    typeContext :: [Name],
    termContext :: [(Name, VType)],
    typeEnv :: TypeEnv,
    typeLevel :: Lvl
  }

typeDefine :: Context -> Name -> VType -> Context
typeDefine (Context tCtx ctx tEnv tLvl) n t = Context (tCtx :> n) ctx (tEnv :> t) (tLvl+1)

typeBind :: Context -> Name -> Context
typeBind (Context tCtx ctx tEnv tLvl) n = Context (tCtx :> n) ctx (tEnv :> VTVar tLvl) (tLvl+1)

-- For terms
bind :: Context -> Name -> VType -> Context
bind (Context tCtx ctx tEnv tLvl) n t = Context tCtx (ctx :> (n, t)) tEnv tLvl

--infixl 4 ~:>
--(~:>) :: Context -> (Name, VType) -> Context
--context ~:> t = context{termContext = termContext context :> t}

--infixl 4 |:>
--(|:>) :: Context -> Name -> Context
--context |:> n = context{typeContext = typeContext context :> n}

infixl 4 :>
pattern (:>) :: [a] -> a -> [a]
pattern xs :> x = x : xs

{-# COMPLETE (:>), [] #-}

newtype Lvl = Lvl Int
  deriving (Num, Eq, Ord, Show)

data VType

  = VTVar Lvl
  | VUnit
  | VFunction VType VType
  | VNat
  | VProduct VType VType
  | VForAll Name (VType -> VType)
  | VInd Name [(Name, VType -> VType)]
  | VIO VType
  | VTChar

type TypeEnv = [VType]

data Val
  = VVar Lvl
  | Todo
-- Needs semantic values

type Env = [Val]
