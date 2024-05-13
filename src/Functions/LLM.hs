module Functions.LLM (llm) where
import Network.Wreq

llm :: IO ()
llm = do
  putStrLn "Test call"
