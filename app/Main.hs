{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text (Text, pack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lucid
import Network.Wai.Handler.Warp (run)
import Poker
import Servant
import Servant.HTML.Lucid
import Test (card)

newtype Result = Result {winner :: Card}

type ComparisonSite = "comparison"

type PokerSite = "poker"

data HomePage = HomePage

type PokerAPI =
  PokerSite :> Get '[HTML] HomePage
    :<|> ComparisonSite :> QueryParam "first" String :> QueryParam "second" String :> Get '[HTML] Result

urlpath :: forall s. (KnownSymbol s) => Text
urlpath = pack $ symbolVal (Proxy :: Proxy s)

instance ToHtml HomePage where
  toHtml HomePage =
    doc_
      ( div_
          ( h1_ "Compare two cards!"
              <> form_
                [action_ (urlpath @ComparisonSite)]
                (inp_ "first" "First Card:")
              <> inp_ "second" "Second Card:"
              <> br_ []
              <> br_ []
              <> button_ "Submit"
          )
      )
  toHtmlRaw = toHtml

instance ToHtml Result where
  toHtml (Result w) = mempty
  toHtmlRaw = toHtml

inp_ :: (Applicative m) => Text -> HtmlT m () -> HtmlT m ()
inp_ id label = label_ [for_ id] label <> br_ [] <> input_ [id_ id, name_ id]

server :: Server PokerAPI
server = return HomePage :<|> comparison
  where
    comparison :: Maybe String -> Maybe String -> Handler Result
    comparison (Just s1) (Just s2) = return $ Result $ max (card s1) (card s2)
    comparison _ _ = return $ Result (card "Ah")

app :: Application
app = serve (Proxy @PokerAPI) server

main :: IO ()
main = run 8080 app

doc_ :: (Applicative m) => HtmlT m () -> HtmlT m ()
doc_ = doctypehtml_ . html_ . body_
