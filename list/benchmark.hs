import Control.Monad (replicateM)

import Ann

main :: IO ()
main = print result
  where
    width = 10
    depth = 10
    values = [0, 0.5, 1]
    ann = fromList (replicate depth width) (sin <$> [1 ..])
    inputs = replicateM width values
    results = execute logsig ann <$> inputs
    result = sum $ concat results
