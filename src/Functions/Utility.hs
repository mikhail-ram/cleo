module Functions.Utility (printRandom) where

import System.Random (randomR, newStdGen)

printRandom :: [String] -> IO ()
printRandom words = do
  g <- newStdGen
  let index = fst $ randomR (0, (length words) - 1) g
  putStrLn $ words !! index
