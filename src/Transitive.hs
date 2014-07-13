{-# LANGUAGE PackageImports #-}
module Transitive where

import Language.Haskell.TH
import Data.Generics
import "mtl" Control.Monad.State
import qualified Data.Set as S

getTypesTransitively :: Name -> Q [(Name, Info)]
getTypesTransitively initial = evalStateT (go initial) (S.empty)
  where
    go n = do
        seen <- get
        if S.member n seen
            then return []
            else do
                put (S.insert n seen)
                minfo <- lift $ recover (return Nothing) (fmap Just (reify n))
                case minfo of
                    Just info@(TyConI dec) -> do
                        fmap (((n, info):) . concat) . mapM go $ listify (\_ -> True) dec
                    _ -> return []
