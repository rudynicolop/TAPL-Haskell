{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Check where

import           AST
import qualified Control.Monad        as CM
import qualified Control.Monad.Except as ERR
import qualified Data.Map.Strict      as M

type Gamma = M.Map Id Type

data R = R Type TExpr

type Result = Either String R

data RP = RP Gamma TPattern

type ResultP = Either String RP

data RF = RF Type [Gamma] [TPattern]

type FoldResult = Either String RF

type RM = (Type, T TExpr)

type MapResult = Either String RM

check :: (Annotation t, Show (t (Pat t)), Show (t (Expr t))) =>
  Gamma -> Expr t -> Result
check g (EName ax) =
  let x = gx ax in do
    case M.lookup x g of
      Nothing -> ERR.throwError $ "Unbound variable " ++ x
      Just t  -> return $ R t $ EName $ T t x
check g EUnit = return $ R TUnit $ EUnit
check g fun@(EFun p t e) = do
  RP g' p' <- checkPattern g t $ gp p
  exhausts <- exhaustive t [p']
  if exhausts then do
    R t' e' <- check g' $ ge e
    return $ R (TFun t t') (EFun (T t p') t (T t' e'))
    else ERR.throwError $ "In function " ++ (show fun) ++
      ", pattern " ++ (show p') ++ " is not exhaustive"
check g app@(EApp e1 e2) = do
  e1' <- check g $ ge e1
  e2' <- check g $ ge e2
  checkapp e1' e2'
  where
    checkapp :: R -> R -> Result
    checkapp (R (TFun t1 t3) e1') (R t2 e2')
      | t1 == t2 = return $ R t3 $ EApp (T (TFun t1 t3) e1') (T t1 e2')
      | otherwise = ERR.throwError $
        "In application " ++ (show app) ++ ", argument " ++ (show e2') ++
          " is expected to have type " ++ (show t1) ++
            ", but has type " ++ (show t2)
    checkapp (R t1 _) _ = ERR.throwError $
      "In application " ++ (show app) ++
        ", expression " ++ (show e1) ++ " has type " ++ (show t1)
check g elet@(ELet p e1 e2) = do
  R t1 e1' <- check g $ ge e1
  RP g' p' <- checkPattern g t1 $ gp p
  exhausts <- exhaustive t1 [p']
  if exhausts then do
    R t2 e2' <- check g' $ ge e2
    return $ R t2 (ELet (T t1 p') (T t1 e1') (T t2 e2'))
  else ERR.throwError $ "In let-expression " ++ (show elet) ++
    ", pattern " ++ (show p') ++ " is not-exhaustive"
check g (EPair e1 e2) = do
  R a e1' <- check g $ ge e1
  R b e2' <- check g $ ge e2
  return $ R (TPair a b) (EPair (T a e1') (T b e2'))
check g efst@(EFst e) = do
  e' <- check g $ ge e
  checkFst e'
  where
    checkFst :: R -> Result
    checkFst (R (TPair a b) e') = return $ R a (EFst (T (TPair a b) e'))
    checkFst (R t' e') = ERR.throwError $
      "In expression " ++ (show efst) ++ ", sub-expression " ++ (show e') ++
        " is expected to have a product type, but has type " ++ (show t')
check g esnd@(ESnd e) = do
  e' <- check g $ ge e
  checkSnd e'
  where
    checkSnd :: R -> Result
    checkSnd (R (TPair a b) e') = return $ R b (EFst (T (TPair a b) e'))
    checkSnd (R t' e') = ERR.throwError $
      "In expression " ++ (show esnd) ++ ", sub-expression " ++ (show e') ++
        " is expected to have a product type, but has type " ++ (show t')
check g left@(ELeft a b e) = do
  R a' e' <- check g $ ge e
  if a' == a
    then return $ R (TEither a b) (ELeft a b (T a e'))
    else ERR.throwError $ "In expression " ++ (show left) ++
      ", sub-expression " ++ (show e') ++ " is expected to have type " ++
      (show a) ++ ", but has type " ++ (show a')
check g right@(ERight a b e) = do
  R b' e' <- check g $ ge e
  if b' == b
    then return $ R (TEither a b) (ELeft a b (T b e'))
    else ERR.throwError $ "In expression " ++ (show right) ++
      ", sub-expression " ++ (show e') ++ " is expected to have type " ++
      (show b) ++ ", but has type " ++ (show b')
check g match@(EMatch e cases) = do
  R t e' <- check g $ ge e
  let (ps,es) = unzip cases in do
    RF _ gs' ps' <- CM.foldM foldHelper (RF t [] []) ps
    exhausts <- exhaustive t ps'
    if exhausts
      then do
        tes <- mapM mapHelper $ zip (reverse gs') es
        let (ts',es') = unzip tes in do
          typeResult <- CM.foldM typeEqual Nothing ts'
          case typeResult of
            Nothing -> do
              ERR.throwError $ "Match-expression " ++
                (show match) ++ " has no cases"
            (Just t') -> do
              let ps'' = map (\p' -> T t p') ps' in
                return $ R t' (EMatch (T t e') $ zip ps'' es')
      else ERR.throwError $ "Match-expression " ++
        (show match) ++ " has inexhaustive patterns"
  where
    foldHelper :: (Annotation t, Show (t (Pat t))) => RF -> t (Pat t) -> FoldResult
    foldHelper (RF t gs ps) p = do
      RP g' p' <- checkPattern g t $ gp p
      return $ RF t (g':gs) (p':ps)
    mapHelper :: (Annotation t, Show (t (Pat t)), Show (t (Expr t))) => (Gamma, t (Expr t)) -> MapResult
    mapHelper (gc,ec) = do
      R t' ec' <- check gc $ ge ec
      return (t', T t' ec')
    typeEqual :: Maybe Type -> Type -> Either String (Maybe Type)
    typeEqual Nothing t = return $ Just t
    typeEqual (Just t') t
      | t' == t = return $ Just t
      | otherwise = ERR.throwError $
        "Case-expressions in match-expression " ++ (show match) ++
          " have different types " ++ (show t) ++ " and " ++ (show t')

checkPattern :: (Annotation t, Show (t (Pat t))) =>
  Gamma -> Type -> Pat t -> ResultP
checkPattern g t (PBase pb) = do
  case gopt pb of
    Nothing  -> return $ RP g (PBase (T t Nothing))
    (Just x) -> return $ RP (M.insert x t g) (PBase (T t (Just x)))
checkPattern g TUnit PUnit = return $ RP g PUnit
checkPattern g (TPair a b) pat@(PPair p1 p2) = do
  RP g1 p1' <- checkPattern g a $ gp p1
  RP g2 p2' <- checkPattern g b $ gp p2
  if M.disjoint g1 g2
    then return $ RP (M.union g1 g2) (PPair (T a p1') (T b p2'))
    else ERR.throwError $ "In pattern " ++ (show pat) ++
      ", sub-patterns " ++ (show p1') ++ " and " ++ (show p2') ++
        " have overlapping variables"
checkPattern g typ@(TEither a' b') pat@(PLeft a b p)
  | a == a' && b == b' = do
    RP g' p' <- checkPattern g a $ gp p
    return $ RP g' (PLeft a b (T a p'))
  | otherwise = ERR.throwError $ "Pattern " ++ (show pat) ++
    " is expected to have type " ++ (show typ) ++
      " but appears to have type " ++ (show (TEither a b))
checkPattern g typ@(TEither a' b') pat@(PRight a b p)
  | a == a' && b == b' = do
    RP g' p' <- checkPattern g b $ gp p
    return $ RP g' (PRight a b (T b p'))
  | otherwise = ERR.throwError $ "Pattern " ++ (show pat) ++
    " is expected to have type " ++ (show typ) ++
      " but appears to have type " ++ (show (TEither a b))
checkPattern g t pat@(POr p1 p2) = do
  RP g1 p1' <- checkPattern g t $ gp p1
  RP g2 p2' <- checkPattern g t $ gp p2
  if g1 == g2 then return $ RP g1 (POr (T t p1') (T t p2'))
    else ERR.throwError $ "In Pattern " ++ (show pat) ++
      ", sub-patterns" ++ (show p1') ++ " and " ++ (show p2') ++
        " are expected to have the same set of variables, but do not"
checkPattern _ t p = do
  ERR.throwError $ "Pattern " ++ (show p) ++
    " does not fit with expected type " ++ (show t)

-- exhaustive patterns
exhaustive :: Type -> [TPattern] -> Either String Bool
exhaustive t ps =
  urec (map (\p -> [patToWpat p]) ps) [PBase (T t ())]

type WPat = Pattern () T

patToWpat :: TPattern -> WPat
patToWpat (PBase (T t _))             = PBase (T t ())
patToWpat PUnit                       = PUnit
patToWpat (PPair (T t1 p1) (T t2 p2)) = PPair (T t1 (patToWpat p1)) (T t2 (patToWpat p2))
patToWpat (PLeft a b (T t p))         = PLeft a b (T t (patToWpat p))
patToWpat (PRight a b (T t p))        = PRight a b (T t (patToWpat p))
patToWpat (POr (T t1 p1) (T t2 p2))   = POr (T t1 (patToWpat p1)) (T t2 (patToWpat p2))

-- URec
urec :: [[WPat]] -> [WPat] -> Either String Bool
urec [] _    = return True
urec pmat []
  | all null pmat = return False
  | otherwise = ERR.throwError "Implementation bug...probs..."
urec _ _ = ERR.throwError "More cases to implement"

-- Complete Signature Sigma
sigma :: Type -> [WPat] -> Bool
sigma TUnit ps = any ((==) PUnit) ps
sigma (TFun _ _) _ = False
sigma (TPair a b) ps = any pairCom ps
  where
    pairCom :: WPat -> Bool
    pairCom (PPair _ _) = True
    pairCom _           = False
sigma (TEither a b) ps = any leftCom ps && any rightCom ps
  where
    leftCom :: WPat -> Bool
    leftCom (PLeft a' b' _) = a' == a && b' == b
    leftCom _               = False
    rightCom :: WPat -> Bool
    rightCom (PRight a' b' _) = a' == a && b' == b
    rightCom _                = False

-- Default Matrix
defM :: [[WPat]] -> Either String [[WPat]]
defM [] = return []
defM ((PBase _ : trow) : ps) = do
  ps' <- defM ps
  return $ trow : ps'
defM ((POr (T _ p1) (T _ p2) : trow) : ps) =
  defM $ (p1 : trow) : (p2 : trow) : ps
defM ((_ : _) : ps) = defM ps
defM _ = ERR.throwError "Oops..."

-- Specialized Matrix

sUnit :: [[WPat]] -> Either String [[WPat]]
sUnit [] = return []
sUnit ((PBase (T TUnit ()) : trow) : ps) = do
  ps' <- sUnit ps
  return $ trow : ps'
sUnit ((PUnit : trow) : ps) = do
  ps' <- sUnit ps
  return $ trow : ps'
sUnit ((POr (T TUnit p1) (T TUnit p2) : trow) : ps) =
  sUnit $ (p1 : trow) : (p2 : trow) : ps
sUnit ((_ : _) : ps) = sUnit ps
sUnit _ = ERR.throwError "Oops..."

sPair :: [[WPat]] -> Either String [[WPat]]
sPair [] = return []
sPair ((PBase (T (TPair a b) ()) : trow) : ps) = do
  ps' <- sPair ps
  return $ (PBase (T a ()) : PBase (T b ()) : trow) : ps'
sPair ((PPair (T a p1) (T b p2) : trow) : ps) = do
  ps' <- sPair ps
  return $ (p1 : p2 : trow) : ps'
sPair ((_ : _) : ps) = sPair ps
sPair _ = ERR.throwError "Oops..."
