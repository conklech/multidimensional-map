{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Map.Multidimensional.Nested
    (NestedMMap
    ,NestedMMapClass()
    )
    where

import Control.Applicative              ((<$>), Applicative, pure)
import Data.Foldable                    (Foldable, foldMap)
import Data.Functor.Identity            (runIdentity)
import Data.Map                         (Map)
import qualified Data.Map               as Map
import Data.Semigroup                   ((<>), mappend, mconcat, mempty, Monoid, Semigroup)
import Data.Traversable                 (Traversable, traverse)
import Data.Vinyl                       (Rec(..), (:::)(..), (=:), PlainRec)

import Data.Map.Multidimensional.Class  (fromList, MMap, toList)

newtype NestedMMap (rs :: [*]) v = NestedMMap { unNestedMMap :: (NMMapType rs v) } 
    deriving (Semigroup, Monoid, Functor, Foldable, Traversable)

class NestedMMapClass (rs :: [*]) where
    data NMMapType rs v :: *
    fromList' :: [(PlainRec rs, v)] -> NMMapType rs v
    toList' :: NMMapType rs v -> [(PlainRec rs, v)]
    -- for basic instances
    mempty' :: NMMapType rs v
    mappend' :: Semigroup v => NMMapType rs v -> NMMapType rs v -> NMMapType rs v
    fmap' :: (a -> b) -> NMMapType rs a -> NMMapType rs b
    foldMap' :: Monoid m => (a -> m) -> NMMapType rs a -> m
    traverse' :: Applicative f => (a -> f b) -> NMMapType rs a -> f (NMMapType rs b)
    
instance (Ord ty) => NestedMMapClass ((sy ::: ty) ': '[]) where
    newtype NMMapType '[(sy ::: ty)] v = NMMBase (Map ty v)
    fromList' = NMMBase . Map.fromList . fmap go
      where
        go :: (PlainRec ((sy ::: ty) ': '[]), v) -> (ty, v)
        go ((tyId :& RNil), v) = (runIdentity tyId, v) 
        go _ = error "impossible pattern match; ghc bug"
    {-# INLINE fromList' #-}
    toList' (NMMBase m) = mconcat $ uncurry go <$> Map.toList m
      where 
        go :: ty -> v -> [(PlainRec '[sy ::: ty], v)] 
        go t v = [(Field =: t, v)]
    {-# INLINE toList' #-}
    fmap' f (NMMBase m) = NMMBase (fmap f m)
    mempty' = NMMBase $ Map.empty
    mappend' (NMMBase m1) (NMMBase m2) = NMMBase $  Map.unionWith (<>) m1 m2
    foldMap' f (NMMBase m) = foldMap f m
    traverse' f (NMMBase m) = NMMBase <$> traverse f m

instance (Ord ty, NestedMMapClass (ks1 ': ks2)) => NestedMMapClass ((sy ::: ty) ': ks1 ': ks2) where
    newtype NMMapType ((sy ::: ty) ': ks1 ': ks2) v = 
        NMMNest (Map ty (NMMapType (ks1 ': ks2) v)) 
    fromList' items = NMMNest $ (fmap fromList') $ Map.fromListWith (++) $ reKey <$> items
      where
        reKey :: (PlainRec ((sy ::: ty) ': ks1 ': ks2), v) -> (ty, [(PlainRec (ks1 ': ks2), v)])
        reKey ((tyId :& ks), v) = (runIdentity tyId, [(ks, v)])
        
    toList' (NMMNest m) = mconcat $ uncurry go <$> Map.toList m
      where 
        go :: ty -> NMMapType (ks1 ': ks2) v -> [(PlainRec ((sy ::: ty) ': ks1 ': ks2), v)] 
        go t m' = fmap (reKey t) $ toList' m' 
        reKey :: ty -> (PlainRec (ks1 ': ks2), v) -> (PlainRec ((sy ::: ty) ': ks1 ': ks2), v)
        reKey t (r, v) = (pure t :& r, v)
    fmap' f (NMMNest m) = NMMNest $ fmap (fmap' f) m
    mempty' = NMMNest $ Map.empty
    mappend' (NMMNest m1) (NMMNest m2) = NMMNest $  Map.unionWith (<>) m1 m2
    foldMap' f (NMMNest m) = foldMap id $ foldMap' f <$> m
    traverse' f (NMMNest m) = NMMNest <$> traverse (traverse' f) m

instance (NestedMMapClass rs) => MMap NestedMMap rs where
    fromList = NestedMMap . fromList'
    toList = toList' . unNestedMMap   

instance (NestedMMapClass rs) => Functor (NMMapType rs) where
    fmap = fmap'
instance (NestedMMapClass rs) => Traversable (NMMapType rs) where
    traverse = traverse'
instance (NestedMMapClass rs) => Foldable (NMMapType rs) where
    foldMap = foldMap'
instance (Semigroup v, NestedMMapClass rs) => Semigroup (NMMapType rs v) where
    (<>) = mappend'
instance (NestedMMapClass rs, Semigroup v) => Monoid (NMMapType rs v) where
    mappend = (<>)
    mempty = mempty'
    
--instance (NestedMMapClass rs, Show (PlainRec rs), Show v) => Show (NMMapType rs v) where
--    showsPrec a b = showParen
--          ((a >= 11)) ((.) (showString "fromList ") (showsPrec 11 $ toList' b))


--class (IElem (sy ::: t) rs) => ApplyMap sy t rs where
--    applyMap' :: PlainRec '[sy ::: Map t (a -> b)] -> NestedMMap rs a -> NestedMMap rs b
    
--instance ApplyMap sy t '[]