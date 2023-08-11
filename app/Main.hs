{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lucid
import Network.Wai.Handler.Warp (run)
import Poker
import Servant
import Servant.HTML.Lucid
import Test (card)

hands :: [String]
hands = map show [High Ace [King, Queen, Jack, Ten, Nine], Pair Ace [King, Queen, Jack, Ten], Pair Ace [King, Queen, Jack, Nine]]

type PokerAPI =
  "poker" :> Get '[JSON] [String]
    :<|> "compare" :> Capture "first" String :> Capture "second" String :> Get '[HTML] (Html ())

instance ToHtml Card where
  toHtml c = span_ (toHtml $ show c)
  toHtmlRaw = toHtml

instance ToHtml [Card] where
  toHtml cs = div_ (foldMap toHtml cs)
  toHtmlRaw = toHtml

server :: Server PokerAPI
server = return hands :<|> comparison
  where
    comparison :: String -> String -> Handler (Html ())
    comparison s1 s2 = do
      let c1 = card s1
          c2 = card s2
          winner = compare c1 c2
      return $ doc_ (toHtml [c1, c2])

app :: Application
app = serve (Proxy @PokerAPI) server

main :: IO ()
main = run 8080 app

doc_ b = doctypehtml_ $ html_ $ body_ $ h1_ "Hello World!" <> b
