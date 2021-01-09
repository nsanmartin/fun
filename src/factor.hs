import System.Environment
import Primes

getIntArgs :: IO [Integer]
getIntArgs = fmap (map read) getArgs

main = do
  ns <- fmap (map read) getArgs
  --print (map factors ns)
  mapM_ (print . (\x -> (x, factors x))) ns
