{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Map.Multidimensional.Flat where

import Data.Vinyl
import Data.Map.Strict                    (Map)
import qualified Data.Map.Strict               as Map

import Data.Map.Multidimensional.Class
import Data.Foldable (Foldable)

newtype FlatMMap rs a = FlatMMap { getFM :: (Map (PlainRec rs) a) }
    deriving (Functor, Foldable, Traversable)
    
-- vinyl-0.1.3 does not actually provide an (Ord (PlainRec rs)) instance.
instance (Ord (PlainRec rs)) => MMap FlatMMap rs where
    fromList = FlatMMap . Map.fromList
    toList = Map.toList . getFM 
    unionWith f (FlatMMap m1) (FlatMMap m2) = FlatMMap $ Map.unionWith f m1 m2
    applyMap field fs (FlatMMap m) = FlatMMap $ Map.mapMaybeWithKey go m
     where
        go k v = fmap ($ v) $ Map.lookup (k ^. rLens field) fs
    