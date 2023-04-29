{-# LANGUAGE TupleSections #-}
module TypeChecker where

import Syntax
import Eval
import Control.Monad.Except
import qualified Data.Map as Map
import Parser

type Checker a = Except (String -> String) a

writeError :: SourceLocation -> String -> Checker a
writeError sl s =
  let
    search :: SourceLocation -> String -> String
    search sl@(SL (1, _) _) cs =  grab sl cs
    search (SL (l1, c1) (l2, c2)) ('\n':cs) = search (SL (l1-1, c1) (l2-1, c2)) cs
    search (SL (l1, c1) (l2, c2)) (_:cs) = search (SL (l1-1, c1) (l2-1, c2)) (decrLine cs)
    search _ [] = "unknown"
    grab :: SourceLocation -> String -> String
    grab (SL (1, s) (1, e)) cs = getLn cs ++ replicate (s - 1) ' ' ++ replicate (e - s) '^'
  in throwError (\f -> s ++ "\n" ++ search sl f)

-- both types must be well formed
conv :: Lvl -> VType -> VType -> Checker ()
conv _ (VTVar n) (VTVar n')
  | n == n' = pure ()
  | otherwise = throwError $ const ("Cannot convert [" ++ show n ++ "] to [" ++ show n' ++ "].")
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
      | otherwise = throwError $ const ("Cannot convert constructors [" ++ c ++ "] and [" ++ c' ++ "].")
conv lvl (VIO a) (VIO a') = conv lvl a a'
conv _ VCharT VCharT = pure ()
conv _ a b = throwError $ const ("Failed to convert (" ++ show a ++ ") -> (" ++ show b ++").")

checkTypeVar :: Context -> Name-> Checker Ix
checkTypeVar (Context [] _ _ _) n = throwError $ const ("Type variable [" ++ n ++ "] not in scope.")
checkTypeVar (Context (theta :> Nothing) gamma x y) n = (+1) <$> checkTypeVar (Context theta gamma x y) n
checkTypeVar (Context (theta :> (Just a)) gamma x y) n
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

inferVar :: Context -> Loc Name -> Checker (Ix, VType)
inferVar (Context _ [] _ _) (L sl n) = writeError sl ("Variable [" ++ n ++ "] not in scope at ")
inferVar (Context theta (gamma :> (Nothing, _)) i j) n = do
  (ix, a) <- inferVar (Context theta gamma i j) n
  pure (ix+1, a)
inferVar (Context theta (gamma :> (Just x, a)) i j) l@(L _ n)
  | x == n = pure (0, a)
  | otherwise = do
      (ix, a) <- inferVar (Context theta gamma i j) l
      pure (ix + 1, a)

infer :: Context -> Loc Raw -> Checker (Term, VType)
infer ctx (L sl (RVar x)) = do
  (x, a) <- inferVar ctx (L sl x)
  pure (Var x, a)
infer _ (L _ RStar) = pure (Star, VUnit)
infer _ (L _ (RNum i)) = pure (Num i, VNat)
infer _ (L _ (RChar c)) = pure (Char c, VCharT)
infer ctx (L sl (RApp t u)) = do
  (t, tType) <- infer ctx t
  case tType of
    VFunction a b -> do
      u <- check ctx u a
      pure (App t u, b)
    _ -> writeError sl "Application head must have function type at "
infer ctx (L _ (RIter c n t0 ts)) = do
  c <- checkType ctx (syntax c)
  let vc = evalType (typeEnv ctx) c
  n <- check ctx n VNat
  t0 <- check ctx t0 vc
  ts <- check ctx ts (VFunction vc vc)
  pure (Iter c n t0 ts, vc)
infer ctx (L sl (RFst t)) = do
  (t, tType) <- infer ctx t
  case tType of
    VProduct a _ -> pure (Fst t, a)
    _ -> writeError sl "Fst projection must have Product type at "
infer ctx (L sl (RSnd t)) = do
  (t, tType) <- infer ctx t
  case tType of
    VProduct _ b -> pure (Snd t, b)
    _ -> writeError sl "Snd projection must have Product type at "
infer ctx (L sl (RFix f t bs)) = do
  t <- checkType ctx (syntax t)
  let vt = evalType (typeEnv ctx) t
  case vt of
    VFunction i@(VInd _ cs) a ->
      let
        consMap = Map.fromList cs
        checkBranch :: Context -> (Loc Name, Binder, Loc Raw) -> Checker (Name, Binder, Term)
        checkBranch eCtx (L csl ci, xi, ti) =
          case consMap Map.!? ci of
            Just consFunc -> do
              ti <- check (bind eCtx xi (consFunc i)) ti a
              pure (ci, xi, ti)
            _ -> writeError csl ("Constructor " ++ ci ++ " is not a constructor for the inductive type at ")
      in do
        bs <- mapM (checkBranch (bind ctx f vt)) bs
        pure (Fix f t bs, vt)
    _ -> writeError sl "Inductive Function has wrong structure at "
infer ctx (L _ (RMatchChar a c cs)) = do
  a <- checkType ctx (syntax a)
  let va = evalType (typeEnv ctx) a
  let
    checkBranch :: (Maybe Char, Loc Raw) -> Checker (Maybe Char, Term)
    checkBranch (c, t) = do
      t <- check ctx t va
      pure (c, t)
  c <- check ctx c VCharT
  cs <- mapM checkBranch cs
  pure (MatchChar a c cs, va)
infer ctx (L _ (RBind x a t u)) = do
  a <- checkType ctx (syntax a)
  let va = evalType (typeEnv ctx) a
  t <- check ctx t (VIO va)
  (u, uType) <- infer (bind ctx x va) u
  pure (Bind x a t u, uType)
infer ctx (L _ (RPrint t)) = do
  t <- check ctx t vtypeString
  pure (Print t, VIO VUnit)
infer _ (L _ (RReadFile s)) = pure (ReadFile s, VIO vtypeString)
infer ctx (L _ (RLet x a t u)) = do
  a <- checkType ctx (syntax a)
  let va = evalType (typeEnv ctx) a
  t <- check ctx t va
  (u, uType) <- infer (bind ctx x va) u
  pure (Let x a t u, uType)
infer ctx (L _ (RLetType t c r)) = do
  c <- checkType ctx (syntax c)
  let vc = evalType (typeEnv ctx) c
  (r, rType) <- infer (typeDefine ctx t vc) r
  pure (LetType t c r, rType)
infer ctx (L sl (RTypeApp t u)) = do
  u <- checkType ctx (syntax u)
  let vu = evalType (typeEnv ctx) u
  (t, tType) <- infer ctx t
  case tType of
    VForAll _ b -> do
      pure (TypeApp t u, b vu)
    _ -> writeError sl "Application head must have function type at "
infer ctx (L _ (RAdd t u)) = do
  vt <- check ctx t VNat
  vu <- check ctx u VNat
  pure (Add vt vu, VNat)
infer ctx (L _ (RMinus t u)) = do
  vt <- check ctx t VNat
  vu <- check ctx u VNat
  pure (Minus vt vu, VNat)
infer ctx (L _ (RTimes t u)) = do
  vt <- check ctx t VNat
  vu <- check ctx u VNat
  pure (Times vt vu, VNat)
infer _ (L sl _) = writeError sl "Inference failed at "

check :: Context -> Loc Raw -> VType -> Checker Term
check ctx (L _ (RPair t u)) (VProduct a b) = do
  t <- check ctx t a
  u <- check ctx u b
  pure (Pair t u)
check ctx (L _ (RAbs x t)) (VFunction a b) = do
  t <- check (bind ctx x a) t b
  pure (Abs x t)
check ctx (L _ (RTypeAbs x t)) (VForAll _ b) = do
  let va = VTVar (typeLevel ctx)
  t <- check (typeBind ctx x) t (b va)
  pure (TypeAbs x t)
check ctx (L sl (RCons c t)) indF@(VInd _ bs) = do
  case Map.fromList bs Map.!? c of
    Just func -> do
      t <- check ctx t (func indF)
      pure (Cons c t)
    _ -> writeError sl ("Constructor " ++ c ++ " is not a constructor of the inductive type at ")
check ctx (L _ (RBind x a t u)) (VIO b) = do
  a <- checkType ctx (syntax a)
  let va = evalType (typeEnv ctx) a
  t <- check ctx t (VIO va)
  u <- check (bind ctx x va) u (VIO b)
  pure (Bind x a t u)
check ctx (L _ (RReturn t)) (VIO a) = do
  t <- check ctx t a
  pure (Return t)
check ctx (L _ (RLet x a t u)) b = do
  a <- checkType ctx (syntax a)
  let va = evalType (typeEnv ctx) a
  t <- check ctx t va
  u <- check (bind ctx x va) u b
  pure (Let x a t u)
check ctx (L _ (RLetType t c r)) b = do
  c <- checkType ctx (syntax c)
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
