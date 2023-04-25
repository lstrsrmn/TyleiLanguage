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
  | CharT
  deriving Show

data Raw
  = RVar Name
  | RStar -- ()
  | RAbs Name Raw -- \Name . Expr
  | RApp Raw Raw -- a b
  | RNum Int
  | RChar Char -- `a`
  | RMatchChar (Type Name) Raw [(Maybe Char, Raw)]
  -- matchChar [type] expr
  -- | 'a' -> ta
  -- | 'b' -> tb
  -- | ...
  -- | _ -> twildcard
  | RIter (Type Name) Raw Raw Raw -- iter [A] n t0 ts
  | RPair Raw Raw -- (a, b)
  | RFst Raw -- fst a
  | RSnd Raw -- snd a
  | RTypeAbs Name Raw -- /\ a . expr
  | RTypeApp Raw (Type Name) -- expr @A
  | RCons Name Raw --
  | RFix Name (Type Name) [(Name, Name, Raw)] -- fix [A]
  -- | Cons1 idk -> expr1
  -- | Cons2 idk -> expr2
  | RBind Name (Type Name) Raw Raw
  | RReturn Raw
  | RLet Name (Type Name) Raw Raw -- let x :: Type = e1 in e2
  | RLetType Name (Type Name) Raw -- letType x :: Type in e

newtype Ix = Ix Int
  deriving (Eq, Ord, Num, Show)


data Term
  = Var Ix
  | Star
  | Abs Name Term
  | App Term Term
  | Num Int
  | Char Char
  | MatchChar (Type Ix) Term [(Maybe Char, Term)]
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
  | VCharT

type TypeEnv = [VType]

data Val
  = VVar Lvl
  | VStar
  | VAbs Name (Val->Val)
  | VApp Val Val
  | VNum Int
  | VChar Char
  | VMatchChar VType Val [(Maybe Char, Val)]
  | VIter VType Val Val Val
  | VPair Val Val
  | VFst Val
  | VSnd Val
  | VTypeAbs Name (VType -> Val)
  | VTypeApp Val VType
  | VCons Name Val
  | VBind Name VType Val (Val -> Val)
  | VReturn Val
  | VFix Name VType [(Name, Name, Val -> Val -> Val)]

type Env = [Val]
