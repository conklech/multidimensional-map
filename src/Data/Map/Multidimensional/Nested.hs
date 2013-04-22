{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE UndecidableInstances, StandaloneDeriving, ScopedTypeVariables, BangPatterns, DeriveFunctor, ImpredicativeTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Map.Multidimensional.Nested
    (HMap
    ,RecordHMap
        ()
    )
    where
--import Data.HList
import Control.Arrow (first)
import GHC.TypeLits
import Data.Functor.Identity (runIdentity)
import Data.Traversable
import Data.Foldable (Foldable, foldMap)
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Semigroup
import Data.Vinyl

import Data.Map.Multidimensional.Class
--class HMap (ks :: [*]) where
--    data HMap ks :: *
    

-- data HMap (ks :: [*]) v where
    -- HTip :: v -> HMap '[] v
    -- HMap :: (Map k (HMap ks v)) -> HMap ((sy ::: k) ': ks) v
--instance Functor (HMap ks) where
--    fmap f (HTip v) = HTip $ f v
--    fmap f (HMap m) = HMap $ fmap f <$> m
--    
--instance Foldable (HMap ks) where
--    foldMap f (HTip v) = f v
--    foldMap f (HMap m) = foldMap (foldMap f) m
--    
--instance (Functor (HMap '[]), Foldable (HMap '[])) => Traversable (HMap '[]) where
--    traverse f (HTip v) = HTip <$> f v
--    {-# INLINE traverse #-}
--instance (Functor (HMap (k ': ks)), Traversable (HMap ks)) => Traversable (HMap (k ': ks)) where
--    traverse f (HMap m) = HMap <$> traverse (traverse f) m
--    {-# INLINE traverse #-}
--




newtype HMap (rs :: [*]) v = HMap { unHMap :: (HMap' rs v) } 
    deriving (Semigroup, Monoid)

class RecordHMap (rs :: [*]) where
    data HMap' rs v :: *
    fromList' :: [(PlainRec rs, v)] -> HMap' rs v
    toList' :: HMap' rs v -> [(PlainRec rs, v)]
    mempty' :: HMap' rs v
    mappend' :: Semigroup v => HMap' rs v -> HMap' rs v -> HMap' rs v

instance (Ord ty) => RecordHMap ((sy ::: ty) ': '[]) where
    newtype HMap' '[(sy ::: ty)] v = HMapBase (Map ty v)
    fromList' = HMapBase . Map.fromList . fmap go
      where
        go :: (PlainRec ((sy ::: ty) ': '[]), v) -> (ty, v)
        go ((tyId :& RNil), v) = (runIdentity tyId, v) 
        go _ = error "impossible pattern match; ghc bug"
    {-# INLINE fromList' #-}
    toList' (HMapBase m) = mconcat $ uncurry go <$> Map.toList m
        where go :: ty -> v -> [(PlainRec '[sy ::: ty], v)] 
              go t v = [(Field =: t, v)]
    {-# INLINE toList' #-}
    mempty' = HMapBase $ Map.empty
    mappend' (HMapBase m1) (HMapBase m2) = HMapBase $  Map.unionWith (<>) m1 m2

instance (Ord ty, RecordHMap (ks1 ': ks2)) => RecordHMap ((sy ::: ty) ': ks1 ': ks2) where
    newtype HMap' ((sy ::: ty) ': ks1 ': ks2) v = HMap'' (Map ty (HMap' (ks1 ': ks2) v)) 
    fromList' items = HMap'' $ (fmap fromList') $ Map.fromListWith (++) $ reKey <$> items
      where
        reKey :: (PlainRec ((sy ::: ty) ': ks1 ': ks2), v) -> (ty, [(PlainRec (ks1 ': ks2), v)])
        reKey ((tyId :& ks), v) = (runIdentity tyId, [(ks, v)])
        
    toList' (HMap'' m) = mconcat $ uncurry go <$> Map.toList m
      where 
        go :: ty -> HMap' (ks1 ': ks2) v -> [(PlainRec ((sy ::: ty) ': ks1 ': ks2), v)] 
        go t m' = fmap (reKey t) $ toList' m' 
        reKey :: ty -> (PlainRec (ks1 ': ks2), v) -> (PlainRec ((sy ::: ty) ': ks1 ': ks2), v)
        reKey t (r, v) = (pure t :& r, v)
    mempty' = HMap'' $ Map.empty
    mappend' (HMap'' m1) (HMap'' m2) = HMap'' $  Map.unionWith (<>) m1 m2

instance (RecordHMap rs) => MMap HMap rs where
    fromList = HMap . fromList'
    toList = toList' . unHMap   

instance (Semigroup v, RecordHMap rs) => Semigroup (HMap' rs v) where
    (<>) = mappend'
instance (RecordHMap rs, Semigroup v) => Monoid (HMap' rs v) where
    mappend = (<>)
    mempty = mempty'
    
instance (RecordHMap rs, Show (PlainRec rs), Show v) => Show (HMap' rs v) where
    showsPrec a b = showParen
          ((a >= 11)) ((.) (showString "fromList ") (showsPrec 11 $ toList' b))

--instance (RecordHMap rs, Monoid v) => Monoid (HMap rs v) where
--    mappend (HMap m1) (HMap m2) = HMap $ mappend m1 m2
--    mempty = HMap mempty

--class (IElem (sy ::: t) rs) => ApplyMap sy t rs where
--    applyMap' :: PlainRec '[sy ::: Map t (a -> b)] -> HMap rs a -> HMap rs b
    
--instance ApplyMap sy t '[]

--{-# INLINE rLensAux #-}
--rLensAux :: forall a r sy t rs. (r ~ (sy ::: t))
--         => r -> Elem r rs -> IndexedTraversal (HMap rs a) (HMap rs b) (Map t a) (Map t b)
--rLensAux _ = go
--  where goHere :: Elem r rs -> Lens' (HMap rs a) (Map t a)
--        goHere Here = lens (\(x :& _) -> x) (\(_ :& xs) x -> x :& xs)
--        goHere _ = error "Unintended base case invocation"
--
--        go :: Elem r rs -> Lens' (HMap rs a) (Map t a)
--        go Here = goHere Here
--        go (There Here) = rLensPrepend $ goHere Here
--        go (There (There Here)) = rLensPrepend $ rLensPrepend $ goHere Here
--        go (There (There (There Here))) =
--          rLensPrepend $ rLensPrepend $ rLensPrepend $ goHere Here
--        go (There (There (There (There Here)))) =
--          rLensPrepend $ rLensPrepend $ rLensPrepend $ rLensPrepend $ goHere Here
--        go (There (There (There (There p)))) =
--          rLensPrepend $ rLensPrepend $ rLensPrepend $ rLensPrepend $ go' p
--        {-# INLINE go #-}
--
--        go' :: Elem r rs' -> Lens' (Rec rs' f) (f t)
--        go' Here = goHere Here
--        go' (There p) = rLensPrepend $ go p
--        {-# INLINABLE go' #-}

--basicSetter :: forall sy a b t. Ord t => Setter (HMap '[(sy:::t)] a) (HMap '[(sy:::t)] b) (Map t a) (Map t b)
--basicSetter = sets go
--  where
--    go :: (Map t a -> Map t b) -> (HMap '[(sy:::t)] a -> HMap '[(sy:::t)] b)
--    go f (HMap m) = HMap $ HTip <$> Map.intersectionWith ($) newMap m
--      where newMap = const <$> f oldMap
--            oldMap = undefined
    

--rLensPrepend :: Setter (HMap rs a) (HMap rs b) (Map t a) (Map t b)
--             -> Setter (HMap (l ': rs) a) (HMap (l ': rs) b) (Map t a) (Map t b)
----rLensPrepend l = lens (\(_ :& xs) -> view l xs) (\(a :& xs) x -> a :& (set l x xs))
--rLensPrepend tr = mapped . tr  
--(\(_ :& xs) -> view l xs) (\(a :& xs) x -> a :& (set l x xs))
--{-# INLINE rLensPrepend #-}
--instance ApplyMap () '[] where
--    applyMap' fs _ | Map.null fs = error "tried to create empty HTip"
--    applyMap' fs (HTip v) = HTip $ (snd $ Map.elemAt 0 fs) v 
--    {-# INLINE applyMap' #-}   
--instance Ord k => ApplyMap k (k ': ks) where
--    applyMap' fs (HMap vs) = HMap $ Map.intersectionWith fmap fs vs
--    {-# INLINE applyMap' #-}
--instance ApplyMap k ks => ApplyMap k (k0 ': ks) where
--    applyMap' fs (HMap vs) = HMap $ fmap (applyMap' fs) vs
--    {-# INLINE applyMap' #-}


--applyMap :: ApplyMap k (k0 ': ks) => Map k (a -> b) -> HMap (k0 ': ks) a -> HMap (k0 ': ks) b
--applyMap = applyMap'
