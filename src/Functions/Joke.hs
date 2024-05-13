{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Functions.Joke (joke) where

import qualified Data.Text as T
import Network.Wreq
import Data.Aeson
import Control.Lens

joke :: IO ()
joke = do
  r <- (get . T.unpack) endpoint
  case decode (r ^. responseBody) of
    Just j  -> print (j :: JokeResponse)
    Nothing -> fail "Joke JSON could not be parsed"
  where
    endpoint = "https://v2.jokeapi.dev/joke/Any?blacklistFlags=nsfw&type=twopart"

data JokeResponse = JokeResponse { setup    :: T.Text
                                 , delivery :: T.Text }

instance Show JokeResponse where
    show response = string
      where
        string = T.unpack . T.concat $ [ setup response
                                       , "\n"
                                       , delivery response ]
    
instance FromJSON JokeResponse where
    parseJSON = withObject "joke response" f
      where
        f o = do
            setup    <- o .: "setup"
            delivery <- o .: "delivery"
            return JokeResponse { setup
                                , delivery }
