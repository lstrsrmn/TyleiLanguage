module Eval where
import Syntax

($$) :: Val -> Val -> Val
(VAbs _ t) $$ a = t a
fix_f@(VFix _ _ bs) $$ cons@(VCons c u) =
  let
    checkBranch :: [(Name, Binder, Val -> Val -> Val)] -> Val
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
evalType env (ForAll a k b) = VForAll a k (\va -> evalType (env :> va) b)
evalType env (Ind t cs) = VInd t (map (\(c,b) -> (c, \vt -> evalType (env :> vt) b)) cs)
evalType env (IO t) = VIO (evalType env t)
evalType env (TypeLamAbs n (L ls a)) = VTypeLamAbs n (L ls (\vb -> evalType (env :> vb) a))
evalType env (TypeLamApp (L ls a) b) =
  let
    va = evalType env a
    vb = evalType env b
  in case va of
    VTypeLamAbs _ (L _ f) -> f vb
    _ -> VTypeLamApp (L ls va) vb

eval :: Env -> TypeEnv -> Term -> Val
eval env _ (Var (Ix x)) = env !! x
eval _ _ Star = VStar
eval env typeEnv (Abs x t) = VAbs x (\vx -> eval (env :> vx) typeEnv t)
eval env typeEnv (App t u) = eval env typeEnv t $$ eval env typeEnv u
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
eval env typeEnv (TypeApp t a) =
  let
    vt = eval env typeEnv t
    va = evalType typeEnv a
  in case vt of
    VTypeAbs _ f -> f va
    _ -> VTypeApp vt va
eval env typeEnv (Cons n t) = VCons n (eval env typeEnv t)
eval env typeEnv (Bind x a t u) =
  let vt = eval env typeEnv t
      va = evalType typeEnv a
  in case vt of
    VReturn t' -> eval (env :> t') typeEnv u
    t' -> VBind x va t' (\vt -> eval (env :> vt) typeEnv u)
eval env typeEnv (Return t) = VReturn (eval env typeEnv t)
eval env typeEnv (Print t) = VPrint (eval env typeEnv t)
eval _ _ (ReadFile s) = VReadFile s
eval env typeEnv (Fix x t bs) =
   let
     evalBranch :: (Name, Binder, Term) -> (Name, Binder, Val -> Val -> Val)
     evalBranch (ci, xi, ti) = (ci, xi, \vx vs -> eval (env :> vx :> vs) typeEnv ti)
   in VFix x (evalType typeEnv t) (map evalBranch bs)
eval env typeEnv (Let _ _ t u) =
  let e1 = eval env typeEnv t
  in eval (env :> e1) typeEnv u
eval env typeEnv (LetType _ a t) =
  let
    i = evalType typeEnv a
  in eval env (typeEnv :> i) t
eval env typeEnv (Add t u) =
  case (eval env typeEnv t, eval env typeEnv u) of
    (VNum i, VNum j) -> VNum (i+j)
    (vt, vu) -> VAdd vt vu
eval env typeEnv (Minus t u) =
  case (eval env typeEnv t, eval env typeEnv u) of
    (VNum i, VNum j) -> VNum (max 0 (i-j))
    (vt, vu) -> VMinus vt vu
eval env typeEnv (Times t u) =
  case (eval env typeEnv t, eval env typeEnv u) of
    (VNum i, VNum j) -> VNum (i*j)
    (vt, vu) -> VTimes vt vu

quoteType :: Lvl -> VType -> Type Ix
quoteType (Lvl l) (VTVar (Lvl x)) = TVar (Ix (l - x - 1))
quoteType _ VUnit = Unit
quoteType lvl (VFunction a b) = Function (quoteType lvl a) (quoteType lvl b)
quoteType _ VNat = Nat
quoteType lvl (VProduct a b) = Product (quoteType lvl a) (quoteType lvl b)
quoteType lvl (VForAll x k f) = ForAll x k (quoteType (lvl+1) (f (VTVar lvl)))
quoteType lvl (VInd x bs) =
  let
    quoteBranch :: (Name, VType -> VType) -> (Name, Type Ix)
    quoteBranch (x, f) = (x, quoteType (lvl+1) (f (VTVar lvl)))
  in Ind x (map quoteBranch bs)
quoteType lvl (VIO a) = IO (quoteType lvl a)
quoteType _ VCharT = CharT
quoteType lvl (VTypeLamAbs n (L ls f)) = TypeLamAbs n (L ls (quoteType (lvl+1) (f (VTVar lvl))))
quoteType lvl (VTypeLamApp (L ls a) b) = TypeLamApp (L ls (quoteType (lvl + 1) a)) (quoteType (lvl+1) b)

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
quote lvl (VPrint t) = Print (quote lvl t)
quote _ (VReadFile s) = ReadFile s
quote lvl (VAdd t u) = Add (quote lvl t) (quote lvl u)
quote lvl (VMinus t u) = Minus (quote lvl t) (quote lvl u)
quote lvl (VTimes t u) = Times (quote lvl t) (quote lvl u)
quote lvl (VFix x a bs) =
  let
    quoteBranch :: (Name, Binder, Val -> Val -> Val) -> (Name, Binder, Term)
    quoteBranch (x, y, f) = (x, y, quote (lvl+2) (f (VVar lvl) (VVar (lvl+1))))
  in Fix x (quoteType lvl a) (map quoteBranch bs)

nbe :: Term -> Term
nbe = quote 0 . eval [] []


class Monad m => ExecEnv m where
  runPrint :: String -> m ()
  runReadFile :: String -> m String

run :: ExecEnv m => Val -> m Val
run (VPrint t) = do
  runPrint (castToStr t)
  pure VStar
  where
    castToStr :: Val -> String
    castToStr (VCons "Nil" _) = []
    castToStr (VCons "Cons" (VPair (VChar '\\') cs)) = '\n':castToStr cs
    castToStr (VCons "Cons" (VPair (VChar c) cs)) = c:castToStr cs
    castToStr _ = error "Error: Blocked term."
run (VReadFile s) = castToVal <$> runReadFile s
  where
    castToVal :: String -> Val
    castToVal [] = VCons "Nil" VStar
    castToVal (c:cs) = VCons "Cons" (VPair (VChar c) (castToVal cs))
run (VReturn t) = pure t
run (VBind _ _ t u) = do
  vt <- run t
  run (u vt)
run v = error ("Error: Blocked term " ++ show (quote 0 v))
