{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses, RankNTypes, TypeOperators, FlexibleContexts #-}
module Data.Map.Multidimensional.Class where

import Data.Vinyl
import Data.Map(Map)

class MMap m (rs :: [*]) where
    fromList :: [(PlainRec rs, a)] -> m rs a
    toList :: m rs a -> [(PlainRec rs, a)]
    applyMap :: forall sy t a b. (IElem (sy ::: t) rs, Ord t) 
             => (sy ::: t)
             -> Map t (a -> b) 
             -> m rs a
             -> m rs b
    applyMap = applyMap' implicitly
    applyMap' :: Elem (sy ::: thisTy) rs 
               -> (sy ::: thisTy)
               -> Map thisTy (a -> b) 
               -> m rs a
               -> m rs b
    unionWith :: (a -> a -> a) 
              -> m rs a 
              -> m rs a  
              -> m rs a
