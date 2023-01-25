module Functions.Time (time)
  where

import Data.Time (getZonedTime, defaultTimeLocale, formatTime)

time :: IO ()
time = do
  currentTime <- getZonedTime
  let formattedTime = formatTime defaultTimeLocale "%I:%M %P" currentTime
  putStrLn $ "It is " ++ formattedTime ++ "."
