{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses, RankNTypes, TypeOperators, FlexibleContexts #-}
module Data.Map.Multidimensional.Class where

import Data.Vinyl
import Data.Map(Map)

class MMap m (rs :: [*]) where
    fromList :: [(PlainRec rs, a)] -> m rs a
    toList :: m rs a -> [(PlainRec rs, a)]
    applyMap :: forall thisSy t a b. IElem (thisSy ::: t) rs 
             => (thisSy ::: t)
             -> Map t (a -> b) 
             -> m rs a
             -> m rs b
