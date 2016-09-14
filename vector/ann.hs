{-# LANGUAGE BangPatterns #-}

module Ann2 where

import qualified Data.Vector.Unboxed as VU

execute :: (Num a, VU.Unbox a) => (a -> a) -> VU.Vector Int -> VU.Vector a -> VU.Vector a -> VU.Vector a
execute sigmoid = go
  where
    go !lengths !weights !values
        | VU.length lengths < 2 = values
        | otherwise = go lengths' weights' values'
      where
        oldCount = VU.unsafeHead lengths + 1
        newCount = lengths VU.! 1
        lengths' = VU.unsafeTail lengths
        weights' = VU.unsafeDrop (oldCount * newCount) weights
        new i = sigmoid $ VU.unsafeHead ws + VU.sum (VU.zipWith (*) (VU.unsafeTail ws) values)
            where ws = VU.unsafeDrop (i * oldCount) weights
        values' = VU.generate newCount new
{-# INLINE execute #-}

step :: (Num a, Ord a) => a -> a
step n = if n > 0 then 1 else 0
{-# INLINE step #-}

logsig :: Floating a => a -> a
logsig n = 1 / (1 + exp (-n))
{-# INLINE logsig #-}
