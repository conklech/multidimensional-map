{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators, FlexibleContexts #-}
import Data.Map.HigherDimensional
import GHC.Vacuum.GraphViz
import Data.Vinyl
import Data.Text
import Data.Map (Map)
import qualified Data.Map as Map

type TestField1 = "one" ::: Text
type TestField2 = "two" ::: Int
testField1 = Field :: TestField1
testField2 = Field :: TestField2
testRecord :: PlainRec '["one" ::: Text, "two" ::: Int]
testRecord = fixRecord $ testField1 =: "hello" <+> testField2 =: 3



testHMap :: HMap '[TestField1, TestField2] Int
testHMap = fromList 
    [(fixRecord $ testField1 =: "hello" <+> testField2 =: 3, 5)
    ,(fixRecord $ testField1 =: "hello" <+> testField2 =: 5, 9)
    ]
--testHMap2 :: HMap '["one" ::: Integer, "two" ::: String, "three" ::: Integer] Int
--testHMap2 = HMap $ Map.fromList 
--    [(3, HMap $ Map.fromList
--        [("hello", HMap $ Map.fromList [(1, HTip 5)])
--        ])
--    ,(4, HMap $ Map.fromList
--        [("hello", HMap $ Map.fromList [(2, HTip 6)])
--        ])
--   ]
testApply :: Map String (Int -> Int)
testApply = Map.fromList
    [("hello", (+11))]
    
--testApply' = applyMap testApply testHMap
testApply2 :: Map String (Int -> Int)
testApply2 = Map.fromList
    [("hello", (*10))]

main = 
    vacuumToSvg "foo" testHMap