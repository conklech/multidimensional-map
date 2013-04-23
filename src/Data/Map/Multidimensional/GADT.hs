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
    (GADTMMap)
    where

import Control.Applicative              ((<$>), Applicative, pure)
import Control.DeepSeq
import Data.Foldable                    (Foldable, foldMap)
import Data.Functor.Identity            (runIdentity)
import Data.Map.Strict                    (Map)
import qualified Data.Map.Strict               as Map
import Data.Semigroup                   ((<>), mappend, mconcat, mempty, Monoid, Semigroup)

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

instance (NFData a, NFData t) 
        => NFData (GADTMMap ((sy ::: t) ': '[]) a) where
    rnf (GADTMMapBase m) = rnf m
    rnf _ = error "impossible; GHC bug 3927"
instance (NFData (GADTMMap rs a), NFData t, NFData a) 
        => NFData (GADTMMap ((sy ::: t) ': rs) a) where
    rnf (GADTMMapNest m) = rnf m 
    rnf _ = error "impossible; GHC bug 3927"
    
instance Ord t => MMap GADTMMap '[sy ::: t] where
    fromList = GADTMMapBase . Map.fromList . fmap stripKey
      where
        stripKey :: (PlainRec ((sy ::: ty) ': '[]), v) -> (ty, v)
        stripKey ((tyId :& RNil), v) = (runIdentity tyId, v) 
        stripKey _ = error "impossible pattern match; ghc bug"
    
    toList (GADTMMapBase m) = mconcat $ uncurry reKey <$> Map.toList m
      where 
        reKey :: ty -> v -> [(PlainRec '[sy ::: ty], v)] 
        reKey t v = [(Field =: t, v)]
    toList _ = error "impossible; GHC bug 3927"
    
    applyMap' e f fs (GADTMMapBase m) = GADTMMapBase $ Map.intersectionWith ($)
                                        (Map.mapKeys (go e f) fs) m
      where
        go :: Elem (thisSy ::: thisTy) ((sy ::: t) ': '[]) -> (thisSy ::: thisTy) -> thisTy -> t
        go Here _ = id
        go _ _ = error "impossible; GHC bug 3927"
        {-# INLINE go #-}
    applyMap' _ _ _  _ = error "impossible; GHC bug 3927"
    {-# INLINE applyMap' #-}
    
    unionWith f (GADTMMapBase m1) (GADTMMapBase m2) = GADTMMapBase $ Map.unionWith f m1 m2
    unionWith _ _ _ = error "impossible; GHC bug 3927" 

instance (Ord t, MMap GADTMMap (rs1 ': rs2)) => MMap GADTMMap ((sy ::: t) ': rs1 ': rs2) where
    fromList items  = GADTMMapNest $ (fmap fromList) $ Map.fromListWith (++) $ stripKey <$> items
      where
        stripKey :: (PlainRec ((sy ::: ty) ': ks1 ': ks2), v) -> (ty, [(PlainRec (ks1 ': ks2), v)])
        stripKey ((tyId :& ks), v) = (runIdentity tyId, [(ks, v)])
        {-# INLINE stripKey #-}

    toList (GADTMMapNest m) = mconcat $ uncurry go <$> Map.toList m
      where 
        go :: ty -> GADTMMap (rs1 ': rs2) v -> [(PlainRec ((sy ::: ty) ': rs1 ': rs2), v)] 
        go = \t m' -> fmap (reKey t) $ toList m'
        {-# INLINE go #-}
        reKey :: ty -> (PlainRec (ks1 ': ks2), v) -> (PlainRec ((sy ::: ty) ': ks1 ': ks2), v)
        reKey t = \(r, v) -> (pure t :& r, v)
        {-# INLINE reKey #-}
    
    applyMap' Here _ fs (GADTMMapNest  m) = GADTMMapNest  $ Map.intersectionWith fmap fs m
    applyMap' (There e) f fs (GADTMMapNest  m) = GADTMMapNest  $ fmap (applyMap' e f fs) m
    {-# INLINE applyMap' #-}
    
    unionWith f (GADTMMapNest m1) (GADTMMapNest m2) = GADTMMapNest $ Map.unionWith (unionWith f) m1 m2
    
instance (MMap GADTMMap rs, Semigroup a) => Semigroup (GADTMMap rs a) where
    (<>) = unionWith (<>)
    
instance (MMap GADTMMap rs, Monoid a) => Monoid (GADTMMap rs a) where
    mappend = unionWith mappend
    mempty = fromList []

-- Constraint crap:

--type family HasConstraint (c :: * -> Constraint) (rs :: [*]) :: Constraint
--type instance HasConstraint c ((sy ::: t) ': '[]) = c t
--type instance HasConstraint c ((sy ::: t) ': rs1 ': rs2) = (c t, HasConstraint c (rs1 ': rs2))

--data GCC (c :: * -> Constraint) (rs :: [*]) where
--    GCCNil :: (c t, r ~ (sy ::: t)) => GCC c (r ': '[])
--    GCCCons :: (c t, r ~ (sy ::: t)) => GCC c rs -> GCC c (r ': rs)
--instance c t => Implicit (GCC c ((sy ::: t) ': '[])) where
--    implicitly = GCCNil 
--instance (c t, IGCC c (rs1 ': rs2)) => Implicit (GCC c ((sy ::: t) ': rs1 ': rs2)) where
--    implicitly = GCCCons $ implicitly

--type IGCC c rs = Implicit (GCC c rs)


--type TestField1 = "one" ::: Int
--type TestField2 = "two" ::: Int
--testField1 :: TestField1
--testField2 :: TestField2
--testField1 = Field
--testField2 = Field
--testHMap :: Int -> Int -> GADTMMap '[TestField1, TestField2] Int
--testHMap x y = fromList $  go x y
--  where go spaceSize itemsPerDimension = 
--            let keys = do
--                x' <- take itemsPerDimension $ [0..]
--                y' <- take itemsPerDimension $ [0..]
--                return (testField1 =: x' <+> testField2 =: y')
--            in keys `zip` [0..]
--test = testHMap 2 2 `deepseq` 0