import System.Environment
import Primes

getIntArgs :: IO [Integer]
getIntArgs = fmap (map read) getArgs

print_result :: [Integer] -> IO ()
print_result [] = putStrLn "No divisors"
print_result ps = putStrLn $ (show ps)

main = do
  ns <- getIntArgs
  mapM_ print_result (map factors ns)
  
