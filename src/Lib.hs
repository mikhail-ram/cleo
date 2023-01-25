module Lib
  ( repl
  ) where

import System.IO (hFlush, stdout)
import Control.Monad (unless)
import Control.Exception (try, SomeException, IOException, evaluate)
import Data.Char (isAlphaNum, isSpace)
import Data.Typeable (typeOf)
import System.Process (callCommand, readProcessWithExitCode, shell, createProcess, readCreateProcessWithExitCode)
import System.Exit (ExitCode (..))

import Functions.Time
import Functions.Weather
import Functions.Utility
import Functions.Joke

prompt :: String
prompt = ">> "

exit :: String
exit = ":q"

introduction :: String
introduction = "This is Cleo, your terminal assistant.\
                \ Cleo learns from your terminal\
                \ commands and starts suggesting\
                \ commands as you need them. When in doubt,\
                \ ask Cleo! Type ':q' anytime to quit."

defaultResponse = [ "Sorry, I don't know how to respond to that."
                  , "Sorry, I don't understand you."
                  , "I don't know how to do that." ]

read' :: IO String
read' = do
  putStr prompt
  hFlush stdout
  getLine

preprocess :: String -> [String]
preprocess input = map (filter isAlphaNum) . words $ input

eval' :: String -> IO ()
{-
eval' input = do
  output <- try (readProcessWithExitCode input [] []) :: IO (Either SomeException (ExitCode, String, String))
  case output of
    Right (ExitSuccess, stdout, _) -> putStrLn stdout
    Right (ExitFailure e, _, stderr) -> putStrLn $ "Process Exited with Exit Code " ++ show e ++ ": " ++ stderr
    Left e -> putStrLn "Error"
-}
eval' input = do
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode (shell input) ""
  case exitcode of
    ExitSuccess -> putStrLn stdout
    ExitFailure e
      | e == 127 -> matchResponse input
      | otherwise -> putStrLn $ stderr ++ "\nExited with Exit Code: " ++ show e

matchResponse :: String -> IO ()
matchResponse input 
  | "time" `elem` tokens = time
  | "weather" `elem` tokens = weather
  | "joke" `elem` tokens = joke
  | otherwise = putStrLn "Sorry, I don't understand."
  where tokens = preprocess input

{-
eval' input = do
  output <- try . callCommand $ input :: IO (Either IOException ())
  case output of
    Right success -> putStrLn "Success"
    Left e
      | "time" `elem` tokens -> time
      | "weather" `elem` tokens -> weather
      | otherwise -> putStrLn "Sorry, I don't understand."
      where tokens = preprocess input
-}
{-
eval' input
  | "time" `elem` tokens = time
  | "weather" `elem` tokens = weather
  | otherwise = do
      output <- try . callCommand $ input :: IO (Either SomeException ())
      case output of
        Left e -> putStrLn "Exception raised."
        Right success -> return success
  where
    tokens = preprocess input
-}

repl :: IO ()
repl = do
  putStrLn introduction
  repl'

repl' :: IO ()
repl' = do
  input <- read'
  unless (input == exit) $ do
    eval' input
    repl'
