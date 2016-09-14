module Ann where

import Data.List (find, foldl')
import Data.List.Split (chunksOf)

type Ann a = [[[a]]]

fromList :: [Int] -> [a] -> Ann a
fromList (x : y : zs) ns = chunksOf (1 + x) front : fromList (y : zs) back
    where (front, back) = splitAt ((1 + x) * y) ns
fromList _ ns = []

execute :: Num a => (a -> a) -> Ann a -> [a] -> [a]
execute f = flip $ foldl' (fmap . (f . sum <$>) . zipWith (*) . (1 :))
{-# INLINE execute #-}

mutations :: Num a => Ann a -> a -> [Ann a]
mutations (((v : zs) : ys) : xs) o
    = (((v + o : zs) : ys) : xs) : (onHead (onHead (v :)) <$> mutations ((zs : ys) : xs) o)
mutations (([] : ys) : xs) o = onHead ([] :) <$> mutations (ys : xs) o
mutations ([] : xs) o = ([] :) <$> mutations xs o
mutations [] _ = []

optimize :: (Num a, Ord b) => (Ann a -> b) -> [a] -> Ann a -> [Ann a]
optimize f os ann
    | Just ann' <- find ((< cost) . f) (mutations ann =<< os) = ann' : optimize f os ann'
    | otherwise = []
    where cost = f ann

hybridize :: Num a => a -> Ann a -> Ann a -> Ann a
hybridize a = zipWith (zipWith (zipWith (\x y -> (1 - a) * x + a * y)))
{-# INLINE hybridize #-}

onHead :: (a -> a) -> [a] -> [a]
onHead f (x : xs) = f x : xs
{-# INLINE onHead #-}

step :: (Num a, Ord a) => a -> a
step n = if n > 0 then 1 else 0
{-# INLINE step #-}

logsig :: Floating a => a -> a
logsig n = 1 / (1 + exp (-n))
{-# INLINE logsig #-}
