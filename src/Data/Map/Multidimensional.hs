{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
module Data.Map.Multidimensional
    (module Data.Map.Multidimensional.Class
    ,module Data.Map.Multidimensional.Nested
    ,fixRecList
    ) where

import Data.Map.Multidimensional.Class
import Data.Map.Multidimensional.Nested

import Control.Applicative
import Control.Arrow
import Data.Vinyl

fixRecList :: [(forall f. Applicative f => Rec rs f, v)] -> [(PlainRec rs, v)]
fixRecList = fmap (first fixRecord)