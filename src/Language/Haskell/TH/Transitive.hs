{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.TH.Transitive where

import qualified "mtl" Control.Monad.State as State
import                 Data.Maybe (isNothing)
import qualified       Data.Set as S
import                 Language.Haskell.TH
import                 Language.Haskell.TH.Transitive.Internal

-- TODO: reifyInstances?

-- | Starting from an initial top level declaration, specified by a
-- 'Name', recursively traverse other related declarations.  The
-- provided function determines whether the current top level
-- declaration should be included in the list of results, and which
-- 'Name's to lookup next.  This function handles keeping track of
-- which 'Name's have already been visited.
reifyTransitively :: ((Name, Info) -> Q (Bool, [Name]))
                    -> Name
                    -> Q [(Name, Info)]
reifyTransitively recurse initial =
    State.evalStateT (go initial) S.empty
  where
    go :: Name -> State.StateT (S.Set Name) Q [(Name, Info)]
    go n = do
        seen <- State.get
        if S.member n seen
            then return []
            else do
                State.put (S.insert n seen)
                minfo <- State.lift $ recover (return Nothing) (fmap Just (reify n))
                case minfo of
                    Just info -> do
                        (shouldEmit, ns) <- State.lift $ recurse (n, info)
                        (if shouldEmit
                             then fmap ((n, info):)
                             else id) $ fmap concat $ mapM go ns
                    _ -> return []

-- | Like 'reifyTransitively', but specialized for transitively
-- traversing concrete datatypes.  This traversal descends through
-- type synonyms, but does not yield them.
--
-- This function is useful for bulk defining typeclass instances like
-- @Binary@, @Lift@, @Data@, @Typeable@, etc.  It isn't very clever,
-- though - it only works well when type constructors mentioned in
-- fields should all have instances defined for them.  It ignores
-- constraints.
getDataTypesTransitively :: ((Name, Info) -> Q Bool)
                         -> Name
                         -> Q [(Name, Info)]
getDataTypesTransitively recursePred = reifyTransitively recurse
  where
    recurse x@(_, TyConI dec@(DataD {})) = recurse' True x dec
    recurse x@(_, TyConI dec@(NewtypeD {})) = recurse' True x dec
    recurse x@(_, TyConI dec@(TySynD {})) = recurse' False x dec
    recurse _ = return (False, [])
    recurse' emit x dec = do
        shouldRecurse <- recursePred x
        return $ if shouldRecurse
                    then (emit, decConcreteNames dec)
                    else (False, [])

-- | Likw 'getDataTypesTransitively', but halts the recursion when a
-- datatype appears to already have an instance for the specified
-- typeclass (which is the first parameter).
--
-- This also takes a function on the name of datatypes, which also
-- halts recursion.  This is used to handle situations where
-- 'instanceMatches' isn't smart enough to determine that an instance exists
getDataTypesWithoutInstance :: Name -> Name -> (Name -> Bool) -> Q [Name]
getDataTypesWithoutInstance clz initial recursePred = do
    insts <- getInstances clz
    let recursePred' (name, _) =
            return $ recursePred name && isNothing (lookupInstance insts name)
    infos <- getDataTypesTransitively recursePred' initial
    return (map fst infos)
