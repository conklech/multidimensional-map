{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators, FlexibleContexts #-}

import Data.Map.Multidimensional
--import GHC.Vacuum.GraphViz
import Data.Vinyl
--import Data.Text
import Data.Map (Map)
import qualified Data.Map as Map

type TestField1 = "one" ::: Int
type TestField2 = "two" ::: Int
testField1 :: TestField1
testField2 :: TestField2
testField1 = Field
testField2 = Field

testHMap :: Int -> NestedMMap '[TestField1, TestField2] Int
testHMap = fromList . go
  where go itemsPerDimension = 
            let keys = do
                x' <- take itemsPerDimension $ [0..]
                y' <- take itemsPerDimension $ [0..]
                return (testField1 =: x' <+> testField2 =: y')
            in keys `zip` [0..]

testApply :: Map Int (Int -> Int)
testApply = Map.fromList
    [(2, (+11))]
    
--testApply' = applyMap testApply testHMap
testApply2 :: Map String (Int -> Int)
testApply2 = Map.fromList
    [("hello", (*10))]

--main = 
--    vacuumToSvg "foo" testHMap