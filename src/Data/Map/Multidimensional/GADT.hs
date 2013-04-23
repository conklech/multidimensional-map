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
{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}

module Data.Map.Multidimensional.GADT
--    (GADTMMap
--    
--    )
    where

import GHC.Exts                         (Constraint)
import Control.Applicative              ((<$>), Applicative, pure)
import Control.DeepSeq
import Data.Foldable                    (Foldable, foldMap)
import Data.Functor.Identity            (runIdentity)
import Data.Map.Strict                    (Map)
import qualified Data.Map.Strict               as Map
import Data.Semigroup                   ((<>), mappend, mconcat, mempty, Monoid, Semigroup)
import Data.Traversable                 (Traversable, traverse)
import Data.Vinyl                       --(Rec(..), (:::)(..), (=:), PlainRec)

import Data.Map.Multidimensional.Class 

data GADTMMap (rs :: [*]) a where
    GADTMMapBase :: Map t a               -> GADTMMap ((sy ::: t) ': '[]) a
    GADTMMapNest :: Map t (GADTMMap rs a) -> GADTMMap ((sy ::: t) ': rs) a
instance Functor (GADTMMap rs) where
    fmap f (GADTMMapBase m) = GADTMMapBase $ fmap f m
    fmap f (GADTMMapNest m) = GADTMMapNest $ fmap (fmap f) m

instance Foldable (GADTMMap rs) where
    foldMap f (GADTMMapBase m) = foldMap f m
    foldMap f (GADTMMapNest m) = foldMap (foldMap f) m
instance Traversable (GADTMMap rs) where
    traverse f (GADTMMapBase m) = GADTMMapBase <$> traverse f m
    traverse f (GADTMMapNest m) = GADTMMapNest <$> traverse (traverse f) m
instance (GC rs, Semigroup a) => Semigroup (GADTMMap rs a) where
    (<>) = unionWith' (<>)

instance GC rs => MMap GADTMMap rs where
    fromList = fromList'
    toList = toList'     
    applyMap = applyMap' implicitly
    
type family HasConstraint (c :: * -> Constraint) (rs :: [*]) :: Constraint
type instance HasConstraint c ((sy ::: t) ': '[]) = c t
type instance HasConstraint c ((sy ::: t) ': rs1 ': rs2) = (c t, HasConstraint c (rs1 ': rs2))

--instance (HasConstraint NFData ((sy ::: t) ': rs), GC rs, NFData a) => 
--    NFData (GADTMMap ((sy ::: t) ': rs) a) where
--instance (HasConstraint NFData rs, GC rs, NFData a) => NFData (GADTMMap rs a) where
--    rnf (GADTMMapBase m) = rnf m
--    rnf (GADTMMapNest m) = rnf m

class GC (rs :: [*]) where
    fromList' :: [(PlainRec rs, v)] -> GADTMMap rs v
    toList' :: GADTMMap rs v -> [(PlainRec rs, v)]  
    applyMap' :: Elem (thisSy ::: thisTy) rs 
               => (thisSy ::: thisTy)
               -> Map thisTy (a -> b) 
               -> GADTMMap rs a
               -> GADTMMap rs b
    unionWith' :: (a -> a -> a) -> GADTMMap rs a -> GADTMMap rs a  -> GADTMMap rs a

instance Ord t => GC '[(sy ::: t)] where
    fromList' = GADTMMapBase . Map.fromList . fmap go
      where
        go :: (PlainRec ((sy ::: ty) ': '[]), v) -> (ty, v)
        go ((tyId :& RNil), v) = (runIdentity tyId, v) 
        go _ = error "impossible pattern match; ghc bug"
    toList' (GADTMMapBase m) = mconcat $ uncurry go <$> Map.toList m
      where 
        go :: ty -> v -> [(PlainRec '[sy ::: ty], v)] 
        go t v = [(Field =: t, v)]
    toList' _ = error "impossible; GHC bug 3927"
    applyMap' e f fs (GADTMMapBase m) = GADTMMapBase $ Map.intersectionWith ($)
         (Map.mapKeys (go e f) fs) m
      where 
        go :: Elem (thisSy ::: thisTy) ((sy ::: t) ': '[]) -> (thisSy ::: thisTy) -> thisTy -> t
        go Here _ = id
        go _ _ = error "impossible; GHC bug 3927"
        {-# INLINE go #-}
    applyMap' _ _ _  _ = error "impossible; GHC bug 3927"
    {-# INLINE applyMap' #-}
    unionWith' f (GADTMMapBase m1) (GADTMMapBase m2) = GADTMMapBase $ Map.unionWith f m1 m2
    unionWith' _ _ _ = error "impossible; GHC bug 3927"

instance (Ord t, GC (ks1 ': ks2)) => GC ((sy ::: t) ': ks1 ': ks2) where
    fromList' items  = GADTMMapNest $ (fmap fromList') $ Map.fromListWith (++) $ reKey <$> items
      where
        reKey :: (PlainRec ((sy ::: ty) ': ks1 ': ks2), v) -> (ty, [(PlainRec (ks1 ': ks2), v)])
        reKey ((tyId :& ks), v) = (runIdentity tyId, [(ks, v)])
        {-# INLINE reKey #-}
    toList' (GADTMMapNest m) = mconcat $ uncurry go <$> Map.toList m
      where 
        go :: ty -> GADTMMap (ks1 ': ks2) v -> [(PlainRec ((sy ::: ty) ': ks1 ': ks2), v)] 
        go = \t m' -> fmap (reKey t) $ toList' m'
        {-# INLINE go #-}
        reKey :: ty -> (PlainRec (ks1 ': ks2), v) -> (PlainRec ((sy ::: ty) ': ks1 ': ks2), v)
        reKey t = \(r, v) -> (pure t :& r, v)
        {-# INLINE reKey #-}
    applyMap' Here _ fs (GADTMMapNest  m)= GADTMMapNest  $ Map.intersectionWith fmap fs m
    applyMap' (There e) f fs (GADTMMapNest  m)= GADTMMapNest  $ fmap (applyMap' e f fs) m
    {-# INLINE applyMap' #-}
    unionWith' f (GADTMMapNest m1) (GADTMMapNest m2) = GADTMMapNest $ Map.unionWith (unionWith' f) m1 m2
--instance (Ord ty, NFData ty, NestedMMapClass (ks1 ': ks2)) => NestedMMapClass ((sy ::: ty) ': ks1 ': ks2) where
--    newtype NMMapType ((sy ::: ty) ': ks1 ': ks2) v = 
--        NMMNest (Map ty (NMMapType (ks1 ': ks2) v)) 
--    fromList' items = NMMNest $ (fmap fromList') $ Map.fromListWith (++) $ reKey <$> items
--      where
--        reKey :: (PlainRec ((sy ::: ty) ': ks1 ': ks2), v) -> (ty, [(PlainRec (ks1 ': ks2), v)])
--        reKey ((tyId :& ks), v) = (runIdentity tyId, [(ks, v)])
--        
--    toList' (NMMNest m) = mconcat $ uncurry go <$> Map.toList m
--      where 
--        go :: ty -> NMMapType (ks1 ': ks2) v -> [(PlainRec ((sy ::: ty) ': ks1 ': ks2), v)] 
--        go t m' = fmap (reKey t) $ toList' m' 
--        reKey :: ty -> (PlainRec (ks1 ': ks2), v) -> (PlainRec ((sy ::: ty) ': ks1 ': ks2), v)
--        reKey t (r, v) = (pure t :& r, v)
--    rnf' (NMMNest m) = rnf m
--    {-# INLINE rnf' #-}
--    fmap' f (NMMNest m) = NMMNest $ fmap (fmap' f) m
--    mempty' = NMMNest $ Map.empty
--    mappend' (NMMNest m1) (NMMNest m2) = NMMNest $  Map.unionWith (<>) m1 m2
--    foldMap' f (NMMNest m) = foldMap id $ foldMap' f <$> m
--    traverse' f (NMMNest m) = NMMNest <$> traverse (traverse' f) m
--    applyMap'' Here _ fs (NMMNest m)= NMMNest $ Map.intersectionWith fmap fs m
--    applyMap'' (There e) f fs (NMMNest m)= NMMNest $ fmap (applyMap'' e f fs) m
--    {-# INLINE applyMap'' #-}
--instance (NestedMMapClass rs) => MMap NestedMMap rs where
--    fromList = NestedMMap . fromList'
--    toList = toList' . unNestedMMap   
--
--instance (NestedMMapClass rs) => Functor (NMMapType rs) where
--    fmap = fmap'
--instance (NestedMMapClass rs) => Traversable (NMMapType rs) where
--    traverse = traverse'
--instance (NestedMMapClass rs) => Foldable (NMMapType rs) where
--    foldMap = foldMap'
--instance (NestedMMapClass rs, NFData a) => NFData (NMMapType rs a) where
--    rnf = rnf'
--instance (Semigroup a, NestedMMapClass rs) => Semigroup (NMMapType rs a) where
--    (<>) = mappend'
--instance (NestedMMapClass rs, Semigroup a) => Monoid (NMMapType rs a) where
--    mappend = (<>)
--    mempty = mempty'
--    
--instance (NestedMMapClass rs, Show (PlainRec rs), Show v) => Show (NestedMMap rs v) where
--    showsPrec a b = showParen
--          ((a >= 11)) ((.) (showString "fromList ") (showsPrec 11 $ toList b))
