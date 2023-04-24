module Eval where
import Syntax
import Lexer
($$) :: Val -> Val -> Val
(VAbs _ t) $$ a = t a
fix_f@(VFix _ _ bs) $$ cons@(VCons c u) =
  let
    checkBranch :: [(Name, Name, Val -> Val -> Val)] -> Val
    checkBranch ((ci, _, ti):bs)
      | ci == c = ti fix_f u
      | otherwise = checkBranch bs
    checkBranch [] = VApp fix_f cons
  in checkBranch bs
f $$ a = VApp f a
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
eval env typeEnv (MatchChar a t bs) =
  let
    evalBranch :: (Maybe Char, Term) -> (Maybe Char, Val)
    evalBranch (mc, t) = (mc, eval env typeEnv t)
    matchBranch :: Val -> [(Maybe Char, Term)] -> Val
    matchBranch (VChar c) ((Just c',t'):cs)
      | c == c' = eval env typeEnv t'
      | otherwise = matchBranch (VChar c) cs
    matchBranch _ ((Nothing, t'):_) = eval env typeEnv t'
    matchBranch vt [] = VMatchChar (evalType typeEnv a) vt (map evalBranch bs)
    matchBranch vt bs = matchBranch vt (tail bs)
  in matchBranch (eval env typeEnv t) bs
eval env typeEnv (Iter c n t0 ts) =
  let
    vc = evalType typeEnv c
    vn = eval env typeEnv n
    vt0 = eval env typeEnv t0
    vts = eval env typeEnv ts
    recurse :: Val -> Val
    recurse (VNum 0) = vt0
    recurse (VNum a) = vts $$ recurse (VNum (a - 1)) 
    recurse _ = VIter vc vn vt0 vts
  in recurse vn
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
eval env typeEnv (Bind x a t u) =
  let vt = eval env typeEnv t
      va = evalType typeEnv a
  in case vt of
    VReturn t' -> eval (env :> t') typeEnv u
    t' -> VBind x va t' (\vt -> eval (env :> vt) typeEnv u)
eval env typeEnv (Return t) = VReturn (eval env typeEnv t)
eval env typeEnv (Fix x t bs) =
   let
     evalBranch :: (Name, Name, Term) -> (Name, Name, Val -> Val -> Val)
     evalBranch (ci, xi, ti) = (ci, xi, \vx vs -> eval (env :> vx :> vs) typeEnv ti)
   in VFix x (evalType typeEnv t) (map evalBranch bs)
eval env typeEnv (Let _ _ t u) =
  let e1 = eval env typeEnv t
  in eval (env :> e1) typeEnv u
eval env typeEnv (LetType _ a t) =
  let
    i = evalType typeEnv a
  in eval env (typeEnv :> i) t

quoteType :: Lvl -> VType -> Type Ix
quoteType (Lvl l) (VTVar (Lvl x)) = TVar (Ix (l - x - 1))
quoteType _ VUnit = Unit
quoteType lvl (VFunction a b) = Function (quoteType lvl a) (quoteType lvl b)
quoteType _ VNat = Nat
quoteType lvl (VProduct a b) = Product (quoteType lvl a) (quoteType lvl b)
quoteType lvl (VForAll x f) = ForAll x (quoteType (lvl+1) (f (VTVar lvl)))
quoteType lvl (VInd x bs) =
  let
    quoteBranch :: (Name, VType -> VType) -> (Name, Type Ix)
    quoteBranch (x, f) = (x, quoteType (lvl+1) (f (VTVar lvl)))
  in Ind x (map quoteBranch bs)
quoteType lvl (VIO a) = IO (quoteType lvl a)
quoteType _ VCharT = CharT

quote :: Lvl -> Val -> Term
quote (Lvl l) (VVar (Lvl x)) = Var (Ix (l - x - 1))
quote _ VStar = Star
quote lvl (VAbs x vt) =
  let
    vx = VVar lvl
    tx = vt vx
  in Abs x (quote (lvl+1) tx)
quote lvl (VApp vt vu) = App (quote lvl vt) (quote lvl vu)
quote _ (VNum n) = Num n
quote _ (VChar c) = Char c
quote lvl (VMatchChar a t bs) =
  let quoteBranch :: (Maybe Char, Val) -> (Maybe Char, Term)
      quoteBranch (mc, t) = (mc, quote lvl t)
  in MatchChar (quoteType lvl a) (quote lvl t) (map quoteBranch bs)
quote lvl (VIter a t u v) = Iter (quoteType lvl a) (quote lvl t) (quote lvl u) (quote lvl v)
quote lvl (VPair t u) = Pair (quote lvl t) (quote lvl u)
quote lvl (VFst t) = Fst (quote lvl t)
quote lvl (VSnd t) = Snd (quote lvl t)
quote lvl (VTypeAbs x f) = TypeAbs x (quote (lvl+1) (f (VTVar lvl)))
quote lvl (VTypeApp t a) = TypeApp (quote lvl t) (quoteType lvl a)
quote lvl (VCons x t) = Cons x (quote lvl t)
quote lvl (VBind x a t f) = Bind x (quoteType lvl a) (quote lvl t) (quote (lvl+1) (f (VVar lvl)))
quote lvl (VReturn t) = Return (quote lvl t)
quote lvl (VFix x a bs) =
  let
    quoteBranch :: (Name, Name, Val -> Val -> Val) -> (Name, Name, Term)
    quoteBranch (x, y, f) = (x, y, quote (lvl+2) (f (VVar lvl) (VVar (lvl+1))))
  in Fix x (quoteType lvl a) (map quoteBranch bs)
