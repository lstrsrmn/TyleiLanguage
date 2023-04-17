module Eval where
import Syntax

evalType :: TypeEnv -> Type Ix ->  VType
evalType env (TVar (Ix x)) = env !! x
evalType _ Unit = VUnit
evalType env (Function a b) = VFunction (evalType env a) (evalType env b)
evalType _ Nat = VNat
evalType env (Product a b) = VProduct (evalType env a) (evalType env b)
evalType env (ForAll a b) = VForAll a (\va -> evalType (env :> va) b)
evalType env (Ind t cs) = VInd t (map (\(c,b) -> (c, \vt -> evalType (env :> vt) b)) cs)
