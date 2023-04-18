{-# LANGUAGE TupleSections #-}
module TypeChecker where

import Syntax
import Eval
import Control.Monad.Except
import qualified Data.Map as Map

type Checker a = Except String a

-- both types must be well formed
conv :: Lvl -> VType -> VType -> Checker ()
conv _ (VTVar n) (VTVar n')
  | n == n' = pure ()
  | otherwise = throwError ("Cannot convert [" ++ show n ++ "] to [" ++ show n' ++ "].")
conv _ VUnit VUnit = pure ()
conv lvl (VFunction a b) (VFunction a' b') = do
  conv lvl a a'
  conv lvl b b'
conv _ VNat VNat = pure ()
conv lvl (VProduct a b) (VProduct a' b') = do
  conv lvl a a'
  conv lvl b b'
conv lvl (VForAll _ b) (VForAll _ b') = do
  conv (lvl + 1) (b (VTVar lvl)) (b' (VTVar lvl))
conv lvl (VInd _ cs) (VInd _ cs') = do
  zipWithM_ convCons cs cs'
  where
    convCons :: (Name, VType -> VType) -> (Name, VType -> VType) -> Checker ()
    convCons (c, b) (c', b')
      | c == c' = conv (lvl + 1) (b (VTVar lvl)) (b' (VTVar lvl))
      | otherwise = throwError ("Cannot convert constructors [" ++ c ++ "] and [" ++ c' ++ "].")
-- conv t t' = throwError ("Failed to convert [" ++ show t ++ "] to [" ++ show t' ++ "].")
conv _ _ _ = throwError "Failed to convert t -> t'"

checkTypeVar :: Context -> Name -> Checker Ix
checkTypeVar (Context [] _ _ _) n = throwError ("Type variable [" ++ n ++ "] not in scope.")
checkTypeVar (Context (theta :> a) gamma x y) n
  | a == n = pure 0
  | otherwise = (+1) <$> checkTypeVar (Context theta gamma x y) n

checkType :: Context -> Type Name -> Checker (Type Ix)
checkType theta (TVar n) = TVar <$> checkTypeVar theta n
checkType _ Unit = pure Unit
checkType theta (Function a b) = do
  a <- checkType theta a
  b <- checkType theta b
  pure (Function a b)
checkType _ Nat = pure Nat
checkType theta (Product a b) = do
  a <- checkType theta a
  b <- checkType theta b
  pure (Product
        a b)
checkType theta (ForAll a b) = do
  b <- checkType (typeBind theta a) b
  pure (ForAll a b)
checkType theta (Ind t cs) = do
  cs <- mapM (\(c, b) -> (c,) <$> checkType (typeBind theta t) b) cs
  pure (Ind t cs)
checkType theta (IO a) = do
  a <- checkType theta a
  pure (IO a)
checkType _ CharT = pure CharT

inferVar :: Context -> Name -> Checker (Ix, VType)
inferVar (Context _ [] _ _) n = throwError ("Variable [" ++ n ++ "] not in scope")
-- broken
inferVar (Context theta (gamma :> (x, a)) i j) n
  | x == n = pure (0, a)
  | otherwise = do
      (ix, a) <- inferVar (Context theta gamma i j) n
      pure (ix + 1, a)

infer :: Context -> Raw -> Checker (Term, VType)
infer ctx (RVar x) = do
  (x, a) <- inferVar ctx x
  pure (Var x, a)
infer _ RStar = pure (Star, VUnit)
infer _ (RNum i) = pure (Num i, VNat)
infer _ (RChar c) = pure (Char c, VCharT)
infer ctx (RApp t u) = do
  (t, tType) <- infer ctx t
  case tType of
    VFunction a b -> do
      u <- check ctx u a
      pure (App t u, b)
    _ -> throwError "Application head must have function type"
infer ctx (RIter c n t0 ts) = do
  c <- checkType ctx c
  let vc = evalType (typeEnv ctx) c
  n <- check ctx n VNat
  t0 <- check ctx t0 vc
  ts <- check ctx ts (VFunction vc vc)
  pure (Iter c n t0 ts, vc)
infer ctx (RFst t) = do
  (t, tType) <- infer ctx t
  case tType of
    VProduct a _ -> pure (Fst t, a)
    _ -> throwError "Fst projection must have Product type."
infer ctx (RSnd t) = do
  (t, tType) <- infer ctx t
  case tType of
    VProduct _ b -> pure (Snd t, b)
    _ -> throwError "Snd projection must have Product type."
infer ctx (RFix f t bs) = do
  t <- checkType ctx t
  let vt = evalType (typeEnv ctx) t
  case vt of
    VFunction (VInd _ cs) a ->
      let
        consMap = Map.fromList cs
        checkBranch :: Context -> (Name, Name, Raw) -> Checker (Name, Name, Term)
        checkBranch eCtx (ci, xi, ti) =
          case consMap Map.!? ci of
            Just consFunc -> do
              ti <- check (bind eCtx xi (consFunc vt)) ti a
              pure (ci, xi, ti)
            _ -> throwError ("Constructor " ++ ci ++ " is not a constructor for the inductive type.")
      in do
        bs <- mapM (checkBranch (bind ctx f vt)) bs
        pure (Fix f t bs, vt)
    _ -> throwError "Inductive Function has wrong structure."
infer ctx (RMatchChar a c cs) = do
  a <- checkType ctx a
  let va = evalType (typeEnv ctx) a
  let
    checkBranch :: (Maybe Char, Raw) -> Checker (Maybe Char, Term)
    checkBranch (c, t) = do
      t <- check ctx t va
      pure (c, t)
  c <- check ctx c VCharT
  cs <- mapM checkBranch cs
  pure (MatchChar a c cs, va)
infer ctx (RBind x a t u) = do
  a <- checkType ctx a
  let va = evalType (typeEnv ctx) a
  t <- check ctx t va
  (u, uType) <- infer (bind ctx x va) u
  pure (Bind x a t u, uType)
infer ctx (RLet x a t u) = do
  a <- checkType ctx a
  let va = evalType (typeEnv ctx) a
  t <- check ctx t va
  (u, uType) <- infer (bind ctx x va) u
  pure (Let x a t u, uType)
infer ctx (RLetType t c r) = do
  c <- checkType ctx c
  let vc = evalType (typeEnv ctx) c
  (r, rType) <- infer (typeDefine ctx t vc) r
  pure (LetType t c r, rType)
infer ctx (RTypeApp t u) = do
  u <- checkType ctx u
  let vu = evalType (typeEnv ctx) u
  (t, tType) <- infer ctx t
  case tType of
    VForAll _ b -> do
      pure (TypeApp t u, b vu)
    _ -> throwError "Application head must have function type"
infer _ _ = throwError "Inference failed."

check :: Context -> Raw -> VType -> Checker Term
check ctx (RPair t u) (VProduct a b) = do
  t <- check ctx t a
  u <- check ctx u b
  pure (Pair t u)
check ctx (RAbs x t) (VFunction a b) = do
  t <- check (bind ctx x a) t b
  pure (Abs x t)
check ctx (RTypeAbs x t) (VForAll _ b) = do
  let va = VTVar (typeLevel ctx)
  t <- check (typeBind ctx x) t (b va)
  pure (TypeAbs x t)
check ctx (RCons c t) indF@(VInd _ bs) = do
  case Map.fromList bs Map.!? c of
    Just func -> do
      t <- check ctx t (func indF)
      pure (Cons c t)
    _ -> throwError ("Constructor " ++ c ++ " is not a constructor of the inductive type/")
check ctx (RBind x a t u) (VIO b) = do
  a <- checkType ctx a
  let va = evalType (typeEnv ctx) a
  t <- check ctx t (VIO va)
  u <- check (bind ctx x va) u (VIO b)
  pure (Bind x a t u)
check ctx (RReturn t) (VIO a) = do
  t <- check ctx t a
  pure (Return t)
check ctx (RLet x a t u) b = do
  a <- checkType ctx a
  let va = evalType (typeEnv ctx) a
  t <- check ctx t va
  u <- check (bind ctx x va) u b
  pure (Let x a t u)
check ctx (RLetType t c r) b = do
  c <- checkType ctx c
  let vc = evalType (typeEnv ctx) c
  r <- check (typeDefine ctx t vc) r b
  pure (LetType t c r)
check ctx ne ty = do
  (ne, neType) <- infer ctx ne
  conv (typeLevel ctx) neType ty
  pure ne

--check ctx t a = do
--  (t, a') <- infer ctx t
--  if conv a a'


-- lambda rule must extend context
-- need a bind function which extends context
