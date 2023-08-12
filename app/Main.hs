{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (FromJSON)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lucid
import Network.Wai.Handler.Warp (run)
import Poker
import Servant
import Servant.HTML.Lucid
import Test (card)
import Web.FormUrlEncoded

newtype Result = Result {evaluationData :: (Double, Double, Double)}

type ComparisonSite = "comparison"

data HomePage = HomePage

data CardForm = CardForm
  { first :: Card,
    second :: Card,
    third :: Card,
    fourth :: Card
  }
  deriving (Generic)

instance FromForm CardForm

instance FromHttpApiData Card where
  parseQueryParam :: Text -> Either Text Card
  parseQueryParam s = maybe (Left "Invalid card") Right (card (unpack s))

type PokerAPI =
  Get '[HTML] HomePage
    :<|> ComparisonSite :> ReqBody '[FormUrlEncoded] CardForm :> Post '[HTML] Result

urlpath :: forall s. (KnownSymbol s) => Text
urlpath = pack $ "/" <> symbolVal (Proxy :: Proxy s)

instance ToHtml HomePage where
  toHtml HomePage =
    doc_
      ( h2_
          "Equity Calculator"
          <> form_
            [action_ (urlpath @ComparisonSite), method_ "POST"]
            ( fieldset_
                ( cardsForm "first" "second" "Seat 1"
                    <> br_ []
                    <> cardsForm "third" "fourth" "Seat 2"
                    <> br_ []
                    <> button_ "Submit"
                )
            )
      )
  toHtmlRaw = toHtml

cardsForm :: (Monad m) => Text -> Text -> Text -> HtmlT m ()
cardsForm id1 id2 title =
  fieldset_
    ( legend_ (toHtml title)
        <> inp_ id1 "First Card:"
        <> inp_ id2 "Second Card:"
    )

instance ToHtml Result where
  toHtml (Result (pw1, pw2, chops)) =
    doc_
      ( p_
          ( toHtml ("Player 1 wins " <> take 4 (show $ pw1 * 100) <> "% of the time")
              <> br_ []
              <> toHtml ("Player 2 wins " <> take 4 (show $ pw2 * 100) <> "% of the time")
              <> br_ []
              <> toHtml ("They chop " <> take 4 (show (100 * chops)) <> "% of the time")
          )
      )
  toHtmlRaw = toHtml

inp_ :: (Applicative m) => Text -> HtmlT m () -> HtmlT m ()
inp_ id label = label_ [for_ id] label <> br_ [] <> input_ [id_ id, name_ id] <> br_ []

server :: Server PokerAPI
server = return HomePage :<|> comparison
  where
    comparison :: CardForm -> Handler Result
    comparison (CardForm c1 c2 c3 c4) = return $ Result $ go [c1, c2] [c3, c4] 3

app :: Application
app = serve (Proxy @PokerAPI) server

main :: IO ()
main = run 8080 app

doc_ :: (Applicative m) => HtmlT m () -> HtmlT m ()
doc_ = doctypehtml_ . html_ . body_
