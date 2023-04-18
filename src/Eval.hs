module Eval where
import Syntax

evalType :: TypeEnv -> Type Ix ->  VType
evalType env (TVar (Ix x)) = env !! x
evalType _ Unit = VUnit
evalType env (Function a b) = VFunction (evalType env a) (evalType env b)
evalType _ Nat = VNat
evalType _ CharT = VCharT
evalType env (Product a b) = VProduct (evalType env a) (evalType env b)
evalType env (ForAll a b) = VForAll a (\va -> evalType (env :> va) b)
evalType env (Ind t cs) = VInd t (map (\(c,b) -> (c, \vt -> evalType (env :> vt) b)) cs)
evalType env (IO t) = VIO (evalType env t)

eval :: Env -> TypeEnv -> Term -> Val
eval env _ (Var (Ix x)) = env !! x
eval _ _ Star = VStar
eval env typeEnv (Abs x t) = VAbs x (\vx -> eval (env :> vx) typeEnv t)
eval env typeEnv (App t u) =
  let vu = eval env typeEnv u
  in case eval env typeEnv t of
    VAbs _ t -> t vu
    vt -> VApp vt vu
eval _ _ (Num n) = VNum n
eval _ _ (Char c) = VChar c
eval env typeEnv (MatchChar a c cs) =
  let
    evalBranch :: (Maybe Char, Term) -> (Maybe Char, Val)
    evalBranch (mc, t) = (mc, eval env typeEnv t)
  in VMatchChar (evalType typeEnv a) (eval env typeEnv c) (map evalBranch cs)
eval env typeEnv (Iter c n t0 ts) =
  let
    ec = evalType typeEnv c
    en = eval env typeEnv n
    et0 = eval env typeEnv t0
    ets = eval env typeEnv ts
  in VIter ec en et0 ets
eval env typeEnv (Pair f s) = VPair (eval env typeEnv f) (eval env typeEnv s)
eval env typeEnv (Fst t) =
  case eval env typeEnv t of
    VPair f _ -> f
    a -> VFst a
eval env typeEnv (Snd t) =
  case eval env typeEnv t of
    VPair _ s -> s
    a -> VSnd a
eval env typeEnv (TypeAbs n t) =
  VTypeAbs n (\va -> eval env (typeEnv :> va) t)
eval env typeEnv (TypeApp t a) = VTypeApp (eval env typeEnv t) (evalType typeEnv a)
eval env typeEnv (Cons n t) = VCons n (eval env typeEnv t)
eval env typeEnv (Bind n t a b) = VBind n (evalType typeEnv t) (eval env typeEnv a) (eval env typeEnv b)
eval env typeEnv (Return t) = VReturn (eval env typeEnv t)
eval env typeEnv (Fix n t bs) = let
  evalBranch :: (Name, Name, Term) -> (Name, Name, Val -> Val)
  evalBranch (c, x, t) = (c, x, \va -> eval (env :> va) typeEnv t)
  in VFix n (evalType typeEnv t) (map evalBranch bs)
eval env typeEnv (Let _ _ t u) =
  let e1 = eval env typeEnv t
  in eval (env :> e1) typeEnv u
eval env typeEnv (LetType _ a t) =
  let
    i = evalType typeEnv a
  in eval env (typeEnv :> i) t
  
