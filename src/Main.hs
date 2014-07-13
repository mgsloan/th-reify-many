module Main where

import Transitive
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Instances
import Language.Haskell.Exts (Module)

main = print $(getTypesTransitively ''Module >>= lift . map (nameBase . fst))