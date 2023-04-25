{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Syntax where

type Name = String
type Binder = Maybe Name

data Type v
  = TVar v
  | Unit
  | Function (Type v) (Type v)
  | Nat
  | Product (Type v) (Type v)
  | ForAll Binder (Type v)
  | Ind Binder [(Name, Type v)] -- ind name with
  -- | Cons Type
  | IO (Type v)
  | CharT
  deriving Show

data Raw
  = RVar Name
  | RStar -- ()
  | RAbs Binder Raw -- \Name . Expr
  | RApp Raw Raw -- a b
  | RNum Int
  | RChar Char -- `a`
  | RMatchChar (Type Name) Raw [(Maybe Char, Raw)]
  -- matchChar [type] expr with
  -- | 'a' -> ta
  -- | 'b' -> tb
  -- | ...
  -- | _ -> twildcard
  | RIter (Type Name) Raw Raw Raw -- iter [A] ( n, t0, ts)
  | RPair Raw Raw -- (a, b)
  | RFst Raw -- fst a
  | RSnd Raw -- snd a
  | RTypeAbs Binder Raw -- /\ a . expr
  | RTypeApp Raw (Type Name) -- expr @A
  | RCons Name Raw -- | Name expr
  | RFix Binder (Type Name) [(Name, Name, Raw)] -- fix f :: A
  -- | Cons1 idk -> expr1
  -- | Cons2 idk -> expr2
  | RBind Binder (Type Name) Raw Raw -- do {x :: A <- t; u}
  | RReturn Raw -- return expr
  | RLet Binder (Type Name) Raw Raw -- let x :: Type = e1 in e2
  | RLetType Binder (Type Name) Raw -- let type x = Type in e

newtype Ix = Ix Int
  deriving (Eq, Ord, Num, Show)


data Term
  = Var Ix 
  | Star
  | Abs Binder Term
  | App Term Term
  | Num Int
  | Char Char
  | MatchChar (Type Ix) Term [(Maybe Char, Term)]
  | Iter (Type Ix) Term Term Term
  | Pair Term Term
  | Fst Term
  | Snd Term
  | TypeAbs Binder Term
  | TypeApp Term (Type Ix)
  | Cons Name Term
  | Bind Binder (Type Ix) Term Term
  | Return Term
  | Fix Binder (Type Ix) [(Name, Name, Term)]
  | Let Binder (Type Ix) Term Term
  | LetType Binder (Type Ix) Term
  deriving (Show)

data Context = Context {
    typeContext :: [Binder],
    termContext :: [(Binder, VType)],
    typeEnv :: TypeEnv,
    typeLevel :: Lvl
  }

typeDefine :: Context -> Binder -> VType -> Context
typeDefine (Context tCtx ctx tEnv tLvl) n t = Context (tCtx :> n) ctx (tEnv :> t) (tLvl+1)

typeBind :: Context -> Binder -> Context
typeBind (Context tCtx ctx tEnv tLvl) n = Context (tCtx :> n) ctx (tEnv :> VTVar tLvl) (tLvl+1)

-- For terms
bind :: Context -> Binder -> VType -> Context
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
  | VForAll Binder (VType -> VType)
  | VInd Binder [(Name, VType -> VType)]
  | VIO VType
  | VCharT

type TypeEnv = [VType]

data Val
  = VVar Lvl
  | VStar
  | VAbs Binder (Val->Val)
  | VApp Val Val
  | VNum Int
  | VChar Char
  | VMatchChar VType Val [(Maybe Char, Val)]
  | VIter VType Val Val Val
  | VPair Val Val
  | VFst Val
  | VSnd Val
  | VTypeAbs Binder (VType -> Val)
  | VTypeApp Val VType
  | VCons Name Val
  | VBind Binder VType Val (Val -> Val)
  | VReturn Val
  | VFix Binder VType [(Name, Name, Val -> Val -> Val)]

type Env = [Val]
