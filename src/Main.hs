{-# LANGUAGE TemplateHaskell #-}

module Main where

import GHC.Real (Ratio)
import Language.Haskell.Exts (Module)
import Language.Haskell.TH
import Language.Haskell.TH.Instances
import Language.Haskell.TH.Lift
import Transitive

deriveLiftTransitively ''Module

-- main = print $(getTypesTransitively ''Module >>= lift . map (nameBase . fst))

-- main = print $(getInstances ''Show >>= lift)

-- instance Show Module where
--    show = undefined

-- main = print $(getTypesTransitively ''Module >>= filterImplemented fst ''Show >>= lift . map (nameBase . fst))
