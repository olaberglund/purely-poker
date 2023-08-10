{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lucid (Html, ToHtml (toHtml), class_, div_, span_, td_, tr_)
import Lucid.Base (ToHtml (toHtmlRaw))
import Network.Wai.Handler.Warp (run)
import Poker
import Servant
import Servant.HTML.Lucid (HTML)
import Test (card)

hands :: [String]
hands = map show [High Ace [King, Queen, Jack, Ten, Nine], Pair Ace [King, Queen, Jack, Ten], Pair Ace [King, Queen, Jack, Nine]]

type PokerAPI =
  "poker" :> Get '[JSON] [String]
    :<|> "compare" :> Capture "first" String :> Capture "second" String :> Get '[JSON] HTML

instance ToHtml Card where
  toHtml c = span_ (toHtml $ show c)
  toHtmlRaw = toHtml

instance ToHtml [Card] where
  toHtml cs = div_ (mconcat $ map toHtml cs)
  toHtmlRaw = toHtml

server :: Server PokerAPI
server =
  return hands
    :<|> comparison
  where
    comparison :: String -> String -> Handler (Html ())
    comparison s1 s2 = do
      let c1 = card s1
          c2 = card s2
          winner = compare c1 c2
      return $ div_ (toHtml $ show c1 <> " vs " <> show c2 <> " winner: " <> show winner)

pokerAPI :: Proxy PokerAPI
pokerAPI = Proxy

app :: Application
app = serve pokerAPI server

main :: IO ()
main = run 8080 app

-- goal : make a web app that takes in two hands and returns the winner
