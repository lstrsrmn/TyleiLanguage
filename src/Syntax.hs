{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Syntax where

type Name = String
type Binder = Maybe Name

data SourceLocation = SL
     { slStart :: (Int, Int)
     , slEnd :: (Int, Int)
     }
instance Functor Loc where
  fmap f (L ls a) = L ls (f a)

data Loc a = L
     { location :: SourceLocation
     , syntax :: a
     }

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
  | RAbs Binder (Loc Raw) -- \Name . Expr
  | RApp (Loc Raw) (Loc Raw) -- a b
  | RNum Int
  | RAdd (Loc Raw) (Loc Raw)
  | RMinus (Loc Raw) (Loc Raw)
  | RTimes (Loc Raw) (Loc Raw)
  | RChar Char -- `a`
  | RMatchChar (Loc (Type Name)) (Loc Raw) [(Maybe Char, Loc Raw)]
  -- matchChar [type] expr with
  -- | 'a' -> ta
  -- | 'b' -> tb
  -- | ...
  -- | _ -> twildcard
  | RIter (Loc (Type Name)) (Loc Raw) (Loc Raw) (Loc Raw) -- iter [A] ( n, t0, ts)
  | RPair (Loc Raw) (Loc Raw) -- (a, b)
  | RFst (Loc Raw) -- fst a
  | RSnd (Loc Raw) -- snd a
  | RTypeAbs Binder (Loc Raw) -- /\ a . expr
  | RTypeApp (Loc Raw) (Loc (Type Name)) -- expr @A
  | RCons Name (Loc Raw) -- | Name expr
  | RFix Binder (Loc (Type Name)) [(Loc Name, Binder, Loc Raw)] -- fix f :: A
  -- | Cons1 idk -> expr1
  -- | Cons2 idk -> expr2
  | RBind Binder (Loc (Type Name)) (Loc Raw) (Loc Raw) -- do {x :: A <- t; u}
  | RReturn (Loc Raw) -- return expr
  | RPrint (Loc Raw) -- print expr
  | RReadFile String -- readFile fileName
  | RLet Binder (Loc (Type Name)) (Loc Raw) (Loc Raw) -- let x :: Type = e1 in e2
  | RLetType Binder (Loc (Type Name)) (Loc Raw) -- let type x = Type in e

newtype Ix = Ix Int
  deriving (Eq, Ord, Num, Show)


data Term
  = Var Ix
  | Star
  | Abs Binder Term
  | App Term Term
  | Num Int
  | Add Term Term
  | Minus Term Term
  | Times Term Term
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
  | Print Term
  | ReadFile String
  | Fix Binder (Type Ix) [(Name, Binder, Term)]
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

instance Show VType where
  show (VTVar _) = "TypeVar"
  show VUnit = "Unit"
  show (VFunction a b) = show a ++ " -> " ++ show b
  show VNat = "Nat"
  show (VProduct a b) = "(" ++ show a ++", " ++ show b ++ ")"
  show (VForAll (Just a) _) = show "forAll " ++ show a
  show (VForAll Nothing _) = "forall _"
  show (VInd (Just a) _) = "ind " ++ show a
  show (VInd Nothing _) = "ind _"
  show (VIO a) = "IO " ++ show a
  show VCharT = "Char"

type TypeEnv = [VType]

data Val
  = VVar Lvl
  | VStar
  | VAbs Binder (Val->Val)
  | VApp Val Val
  | VNum Int
  | VAdd Val Val
  | VMinus Val Val
  | VTimes Val Val
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
  | VPrint Val
  | VReadFile String
  | VFix Binder VType [(Name, Binder, Val -> Val -> Val)]

type Env = [Val]

vtypeString :: VType
vtypeString = VInd (Just "String") [("StrNil",  const VUnit), ("StrCons", VProduct VCharT)]
