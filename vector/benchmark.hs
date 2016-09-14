import Control.Monad
import qualified Data.Vector.Unboxed as VU

import Ann

main :: IO ()
main = print result
  where
    width = 10
    depth = 10
    values = [0, 0.5, 1] :: [Double]
    ann = VU.generate (width * (width + 1) * (depth - 1)) (sin . fromIntegral)
    inputs = VU.replicateM width values
    results = execute logsig (VU.replicate depth width) ann <$> inputs
    result = sum $ VU.sum <$> results
