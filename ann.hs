module Ann where

import Data.List (foldl')
import Data.List.Split (chunksOf)

type Ann a = [[[a]]]

fromList :: [Int] -> [a] -> Ann a
fromList (x : y : zs) ns = chunksOf (1 + x) front : fromList (y : zs) back
    where (front, back) = splitAt ((1 + x) * y) ns
fromList _ ns = []

execute :: Num a => (a -> a) -> Ann a -> [a] -> [a]
execute f = flip $ foldl' (fmap . (f . sum <$>) . zipWith (*) . (1 :))
{-# INLINE execute #-}

step :: (Num a, Ord a) => a -> a
step n = if n > 0 then 1 else 0
{-# INLINE step #-}

logsig :: Floating a => a -> a
logsig n = 1 / (1 + exp (-n))
{-# INLINE logsig #-}
