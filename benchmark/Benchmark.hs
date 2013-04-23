{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, MultiParamTypeClasses, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module Main where

import Data.Foldable (Foldable, foldMap)
import Control.DeepSeq
import Data.Vinyl
import Control.Exception (evaluate)

import Criterion
import Criterion.Main
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import Data.Monoid

import Data.Map.Multidimensional
import Data.Map.Multidimensional.GADT
--import Data.Text hiding (zip)

type TestField1 = "one" ::: Int
type TestField2 = "two" ::: Int
testField1 :: TestField1
testField2 :: TestField2
testField1 = Field
testField2 = Field
--testRecord :: PlainRec '["one" ::: Int, "two" ::: Int]
--testRecord = fixRecord $ testField1 =: "hello" <+> testField2 =: 3

type MMC rs a = forall m. (MMap m rs, Foldable (m rs)) => m rs a 
type Fields = '[TestField1, TestField2]
testHMap :: Int -> Int -> MMC  '[TestField1, TestField2] Int
testHMap x y = fromList $  go x y
  where go spaceSize itemsPerDimension = 
            let keys = do
                x' <- take itemsPerDimension $ randomRs (0,spaceSize) $ mkStdGen 1
                y' <- take itemsPerDimension $ randomRs (0,spaceSize) $ mkStdGen 2
                return (testField1 =: x' <+> testField2 =: y')
            in keys `zip` [0..]
            
factorMap :: Int -> Int -> Map Int (Int -> Int)
factorMap spaceSize items = force $
    Map.fromList [(k,(+k))| k <- take items $ randomRs (0,spaceSize) $ mkStdGen 0]
    
nested :: Int -> Int -> NestedMMap '[TestField1, TestField2] Int
nested n1 n2 = testHMap n1 n2
test :: forall m. (MMap m '[TestField1, TestField2], Foldable (m '[TestField1, TestField2])) 
      => Map Int (Int -> Int) -> m '[TestField1, TestField2] Int -> Int
test x y = getSum $ foldMap Sum $ applyMap testField1 x y   
main = defaultMain
    [bench "nested" $ whnf (test $ factorMap 30 30) $ (testHMap 30 30 :: NestedMMap Fields Int)
    ,bench "GADT  " $ whnf (test $ factorMap 30 30) $ (testHMap 30 30 :: GADTMMap Fields Int)
    ]
--    input <- evaluate $ force $ nested 60 100
--    defaultMain
--        [bench "wh-id" $ whnf id input
--        ,bench "wh-notforced" $ whnf (applyMap testField1 (factorMap 60 100)) $ nested 60 100
--        ,bench "wh-forced" $ whnf (applyMap testField1 (factorMap 60 100)) $ input
--        ,bench "id" $ nf id input
--        ,bench "forced" $ nf (applyMap testField1 (factorMap 60 100)) $ input
--        ,bench "notforced" $ nf (applyMap testField1 (factorMap 60 100)) $ nested 60 100
--         
--        ]
-----
--    [bench "30 : 10*10  " $ nf (flip (applyMap testField1) $ nested 30 10) (factorMap 30 10) 
--    ,bench "60 : 10*10  " $ nf (flip (applyMap testField1) $ nested 60 10) (factorMap 60 10) 
--    ,bench "90 : 10*10  " $ nf (flip (applyMap testField1) $ nested 90 10) (factorMap 90 10)
--    ,bench "30 : 10*10 " $ nf (flip (applyMap testField1) $ nested 30 10) (factorMap 30 10)
--    ,bench "30 : 100*100 " $ nf (flip (applyMap testField1) $ nested 30 100) (factorMap 30 100)
--    ,bench "30 : 1000*1000" $ nf (flip (applyMap testField1) $ nested 30 1000) (factorMap 30 1000)
--    ,bench "1000 : 100*100" $ nf (flip (applyMap testField1) $ nested 1000 100) (factorMap 1000 100)
--    ]