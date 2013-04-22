{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses #-}
module Data.Map.Multidimensional.Class where

import Data.Vinyl

class MMap m (rs :: [*]) where
    fromList :: [(PlainRec rs, a)] -> m rs a
    toList :: m rs a -> [(PlainRec rs, a)]
    