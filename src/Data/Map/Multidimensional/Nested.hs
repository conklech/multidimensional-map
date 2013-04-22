{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Map.Multidimensional.Nested
    (NestedMMap
    ,NestedMMapClass(applyMap)
    )
    where

import Control.Applicative              ((<$>), Applicative, pure)
import Control.DeepSeq
import Data.Foldable                    (Foldable, foldMap)
import Data.Functor.Identity            (runIdentity)
import Data.Map.Strict                    (Map)
import qualified Data.Map.Strict               as Map
import Data.Semigroup                   ((<>), mappend, mconcat, mempty, Monoid, Semigroup)
import Data.Traversable                 (Traversable, traverse)
import Data.Vinyl                       --(Rec(..), (:::)(..), (=:), PlainRec)

import Data.Map.Multidimensional.Class  (fromList, MMap, toList)

newtype NestedMMap (rs :: [*]) v = NestedMMap { unNestedMMap :: (NMMapType rs v) } 
    deriving (Semigroup, Monoid, Functor, Foldable, Traversable, NFData)

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
    rnf' :: NFData a => NMMapType rs a -> ()
    applyMap :: forall thisSy t a b. IElem (thisSy ::: t) rs 
             => (thisSy ::: t)
             -> Map t (a -> b) 
             -> NestedMMap rs a
             -> NestedMMap rs b
    applyMap s m = NestedMMap . applyMap' s m . unNestedMMap
    {-# INLINE applyMap #-}
    applyMap' :: forall thisSy t a b. IElem (thisSy ::: t) rs 
              => (thisSy ::: t)
              -> Map t (a -> b) 
              -> NMMapType rs a 
              -> NMMapType rs b
    applyMap' = applyMap'' implicitly
    {-# INLINE applyMap' #-}
    applyMap'' :: Elem (thisSy ::: thisTy) rs 
               => (thisSy ::: thisTy)
               -> Map thisTy (a -> b) 
               -> NMMapType rs a
               -> NMMapType rs b
               
    
instance (Ord ty, NFData ty) => NestedMMapClass ((sy ::: ty) ': '[]) where
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
    applyMap'' e f fs (NMMBase m) = NMMBase $ Map.intersectionWith ($)
         (Map.mapKeys (go e f) fs) m
      where 
        go :: Elem (thisSy ::: t) ((sy ::: ty) ': '[]) -> (thisSy ::: t) -> t -> ty
        go Here _ = id
        {-# INLINE go #-}
    {-# INLINE applyMap'' #-}
    rnf' (NMMBase m) = rnf m
    {-# INLINE rnf' #-}
    fmap' f (NMMBase m) = NMMBase (fmap f m)
    mempty' = NMMBase $ Map.empty
    mappend' (NMMBase m1) (NMMBase m2) = NMMBase $  Map.unionWith (<>) m1 m2
    foldMap' f (NMMBase m) = foldMap f m
    traverse' f (NMMBase m) = NMMBase <$> traverse f m

instance (Ord ty, NFData ty, NestedMMapClass (ks1 ': ks2)) => NestedMMapClass ((sy ::: ty) ': ks1 ': ks2) where
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
    rnf' (NMMNest m) = rnf m
    {-# INLINE rnf' #-}
    fmap' f (NMMNest m) = NMMNest $ fmap (fmap' f) m
    mempty' = NMMNest $ Map.empty
    mappend' (NMMNest m1) (NMMNest m2) = NMMNest $  Map.unionWith (<>) m1 m2
    foldMap' f (NMMNest m) = foldMap id $ foldMap' f <$> m
    traverse' f (NMMNest m) = NMMNest <$> traverse (traverse' f) m
    applyMap'' Here _ fs (NMMNest m)= NMMNest $ Map.intersectionWith fmap fs m
    applyMap'' (There e) f fs (NMMNest m)= NMMNest $ fmap (applyMap'' e f fs) m
    {-# INLINE applyMap'' #-}
instance (NestedMMapClass rs) => MMap NestedMMap rs where
    fromList = NestedMMap . fromList'
    toList = toList' . unNestedMMap   

instance (NestedMMapClass rs) => Functor (NMMapType rs) where
    fmap = fmap'
instance (NestedMMapClass rs) => Traversable (NMMapType rs) where
    traverse = traverse'
instance (NestedMMapClass rs) => Foldable (NMMapType rs) where
    foldMap = foldMap'
instance (NestedMMapClass rs, NFData a) => NFData (NMMapType rs a) where
    rnf = rnf'
instance (Semigroup a, NestedMMapClass rs) => Semigroup (NMMapType rs a) where
    (<>) = mappend'
instance (NestedMMapClass rs, Semigroup a) => Monoid (NMMapType rs a) where
    mappend = (<>)
    mempty = mempty'
    
--instance (NestedMMapClass rs, Show (PlainRec rs), Show v) => Show (NMMapType rs v) where
--    showsPrec a b = showParen
--          ((a >= 11)) ((.) (showString "fromList ") (showsPrec 11 $ toList' b))


--class (IElem (sy ::: t) rs) => ApplyMap sy t rs where
--    applyMap' :: PlainRec '[sy ::: Map t (a -> b)] -> NestedMMap rs a -> NestedMMap rs b
    
--instance ApplyMap sy t '[]