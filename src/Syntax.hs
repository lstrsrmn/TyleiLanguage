{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Syntax where

type Name = String
type Binder = Maybe Name

data SourceLocation = SL
     { slStart :: (Int, Int)
     , slEnd :: (Int, Int)
     }
  deriving Show
instance Functor Loc where
  fmap f (L ls a) = L ls (f a)

data Loc a = L
     { location :: SourceLocation
     , syntax :: a
     }
instance (Show a) => Show (Loc a) where
  show (L _ a) = show a

data Kind
  = Type
  | TypeFunction (Loc Kind) (Loc Kind)

instance Show Kind where
  show Type = "Type"
  show (TypeFunction a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

instance Eq Kind where
  (==) Type Type = True
  (==) (TypeFunction (L _ a) (L _ b)) (TypeFunction (L _ c) (L _ d)) = (a == c) && (b == d)
  (==) _ _ = False

(=>>) :: SourceLocation -> SourceLocation -> SourceLocation
(=>>) (SL s _ ) (SL _ e) = SL s e

data Type v
  = TVar v
  | Unit
  | Function (Type v) (Type v)
  | Nat
  | Product (Type v) (Type v)
  | ForAll Binder (Loc Kind) (Type v) -- forall f :: Kind . Type
  | Ind Binder [(Name, Type v)] -- ind name with
  | TypeLamAbs Binder (Loc (Type v))
  | TypeLamApp (Loc (Type v)) (Type v)
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
  | RLetType Binder (Loc Kind) (Loc (Type Name)) (Loc Raw) -- let type x :: Kind = Type in e

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

instance Show Term where
  show (Var i) = "Var " ++ show i
  show Star = "()"
  show (Abs (Just n) t) = "\\" ++ n ++ ". " ++ show t ++ "\n"
  show (Abs Nothing t) = "\\_. " ++ show t ++ "\n"
  show (App a b) = show a ++ " " ++ show b
  show (Num i) = show i
  show (Add a b) = show a ++ " + " ++ show b
  show (Minus a b) = show a ++ " - " ++ show b
  show (Times a b) = show a ++ " * " ++ show b
  show (Char c) = [c]
  show (MatchChar a t _) = "matchChar ::" ++ show a ++ " on " ++ show t ++ " with branches ... \n"
  show (Iter a t1 t2 t3) = "iter :: " ++ show a ++ " " ++ show t1 ++ " " ++ show t2 ++ " " ++ show t3 ++ "\n"
  show (Pair a b) = "(" ++ show a ++ "," ++ show b ++ ")"
  show (Fst a) = "fst " ++ show a
  show (Snd a) = "snd " ++ show a
  show (TypeAbs n t) = "\\" ++ show n ++ ". " ++ show t ++ "\n"
  show (TypeApp t u) = show t ++ " " ++ show u
  show (Cons a t) = a ++ " " ++ show t
  show (Bind (Just x) a t u) = "{do " ++ x ++ " :: " ++ show a ++ " <- " ++ show t ++ "; " ++ show u ++ "}\n"
  show (Bind Nothing a t u) = "{do _  :: " ++ show a ++ " <- " ++ show t ++ "; " ++ show u ++ "}\n"
  show (Return t) = "return " ++ show t
  show (Print t) = "print " ++ show t
  show (ReadFile s) = "readFile " ++ s
  show (Fix (Just x) t _) = "fix " ++ x ++ " :: " ++ show t ++ " branches ...\n"
  show (Fix Nothing t _) = "fix _ :: " ++ show t ++ " branches ...\n"
  show (Let (Just x) a t u) = "let " ++ x ++ " :: " ++ show a ++ " = " ++ show t ++ " in " ++ show u ++ "\n"
  show (Let Nothing a t u) = "let _ :: " ++ show a ++ " = " ++ show t ++ " in " ++ show u ++ "\n"
  show (LetType (Just x) a u) = "let type " ++ x ++ " = " ++ show a ++ " in " ++ show u ++ "\n"
  show (LetType Nothing a u) = "let type _ = " ++ show a ++ " in " ++ show u ++ "\n"

data Context = Context {
    typeContext :: [(Binder, Kind)],
    termContext :: [(Binder, VType)],
    typeEnv :: TypeEnv,
      typeLevel :: Lvl
  }

typeDefine :: Context -> Binder -> VType -> Kind -> Context
typeDefine (Context tCtx ctx tEnv tLvl) n t k = Context (tCtx :> (n, k)) ctx (tEnv :> t) (tLvl+1)

typeBind :: Context -> Binder -> Kind -> Context
typeBind (Context tCtx ctx tEnv tLvl) n k = Context (tCtx :> (n, k)) ctx (tEnv :> VTVar tLvl) (tLvl+1)

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
  | VForAll Binder (Loc Kind) (VType -> VType)
  | VInd Binder [(Name, VType -> VType)]
  | VIO VType
  | VTypeLamAbs Binder (Loc (VType -> VType))
  | VTypeLamApp (Loc VType) VType
  | VCharT

instance Show VType where
  show (VTVar _) = "TypeVar"
  show VUnit = "Unit"
  show (VFunction a b) = show a ++ " -> " ++ show b
  show VNat = "Nat"
  show (VProduct a b) = "(" ++ show a ++", " ++ show b ++ ")"
  show (VForAll (Just a) _ _) = show "forAll " ++ show a
  show (VForAll Nothing _ _) = "forall _"
  show (VInd (Just a) _) = "ind " ++ show a
  show (VInd Nothing _) = "ind _"
  show (VIO a) = "IO " ++ show a
  show VCharT = "Char"
  show (VTypeLamAbs n _) = "\\" ++ show n ++ ". "
  show (VTypeLamApp a b) = show a ++ " " ++ show b

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
vtypeString = VInd (Just "String") [("Nil",  const VUnit), ("Cons", VProduct VCharT)]
