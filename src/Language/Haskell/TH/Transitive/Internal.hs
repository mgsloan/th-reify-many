{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.TH.Transitive.Internal where

import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Safe (headMay, tailMay)

-- | For data, newtype, and type declarations, yields a list of the
-- types of the fields.  In the case of a type synonyms, it just
-- returns the body of the type synonym as a singleton list.
decToFieldTypes :: Dec -> [[Type]]
decToFieldTypes (DataD _ _ _ cons _) = map conToFieldTypes cons
decToFieldTypes (NewtypeD _ _ _ con _) = [conToFieldTypes con]
decToFieldTypes (TySynD _ _ ty) = [[ty]]
decToFieldTypes _ = []

-- | Yields the types of the fields of the constructor.
conToFieldTypes :: Con -> [Type]
conToFieldTypes (NormalC _ xs) = map snd xs
conToFieldTypes (RecC _ xs) = map (\(_, _, ty) -> ty) xs
conToFieldTypes (InfixC (_, ty1) _ (_, ty2)) = [ty1, ty2]
conToFieldTypes (ForallC _ _ con) = conToFieldTypes con

-- | Pulls out the names of all type constructors which aren't
-- involved in constraints.
typeConcreteNames :: Type -> [Name]
typeConcreteNames (ForallT _ _ ty) = typeConcreteNames ty
typeConcreteNames (AppT l r) = typeConcreteNames l ++ typeConcreteNames r
typeConcreteNames (SigT ty _) = typeConcreteNames ty
typeConcreteNames (ConT n) = [n]
typeConcreteNames _ = []

-- | Pulls out the names of all type constructors used when defining
-- type constructors.
decConcreteNames :: Dec -> [Name]
decConcreteNames = concatMap (concatMap typeConcreteNames) . decToFieldTypes

data TypeclassInstance = TypeclassInstance Cxt Type [Dec]
    deriving Show

getInstances :: Name -> Q [TypeclassInstance]
getInstances clz = do
    res <- reify clz
    case res of
        ClassI _ xs -> return $ mapMaybe convertDec xs
        _ -> fail $ "Error in getInstances: " ++ show clz ++ " isn't a class"
  where
    convertDec (InstanceD ctxt typ decs) = Just (TypeclassInstance ctxt typ decs)
    convertDec _ = Nothing

lookupInstance :: [TypeclassInstance] -> Name -> Maybe TypeclassInstance
lookupInstance xs n = headMay $ filter (`instanceMatches` n) xs

instanceMatches :: TypeclassInstance -> Name -> Bool
instanceMatches (TypeclassInstance _ typ _) n' =
    case tailMay $ map (headMay . unAppsT) $ unAppsT typ  of
        Nothing -> False
        Just xs -> not $ null [() | Just (ConT n) <- xs, n == n']

unAppsT :: Type -> [Type]
unAppsT = go []
  where
    go xs (AppT l x) = go (x : xs) l
    go xs ty = ty : xs
