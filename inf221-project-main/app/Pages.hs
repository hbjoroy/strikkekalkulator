{-# LANGUAGE OverloadedStrings, BlockArguments, DataKinds, RecordWildCards, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-} --nødvendig orphan yarn og yarnamount for å unngå syklisk import

module Pages where

import Lucid
import DB
import Data.Text hiding (map)
import Database.Selda
import Web.FormUrlEncoded
import qualified Data.Text as T

data HomePage = HomePage

instance ToHtml HomePage where
  toHtml = toHtml . toHtmlRaw 
  toHtmlRaw HomePage = template do
    h1_ "Strikkekalkulator"
    h4_ "Sjekk garnet eller regn ut garnbytte."
    p_ $ do
      "Sjekk om en type garn finnes i databasen, se garndata: "
      a_ [href_ "/yarn/Alpakka%20Følgetråd"] "Garn-info"
    p_ $ do
      "Regn ut hvor mange nøster du trenger hvis du bytter garn: "
      a_ [href_ "/kalkulator"] "Kalkulator"

instance ToHtml Yarn where
    toHtml = toHtml . toHtmlRaw 
    toHtmlRaw Yarn{..} = template do
        h2_ $ toHtml name
        p_ do
            "Løpelengde (meterage): "
            toHtml $ show meterage
            " m"
        p_ do
            "Pris per nøste: "
            toHtml $ show price
            " kr"
        p_ do
            "Strikkefasthet: "
            toHtml $ show strikkefasthet
            "m/10cm"

        img_ [src_ "https://www.crochet365knittoo.com/wp-content/uploads/2017/03/Yarn-weights.webp", style_ "max-width: 100%"]

instance ToHtml [YarnAmount] where
  toHtml = toHtml . toHtmlRaw
  toHtmlRaw yas =
    toHtmlRaw $
      T.intercalate ", " $
        map (\(y, n) -> name y <> ": " <> T.pack (show n) <> " nøste(r)") yas

-- | Startsiden for garnbyttekalkulator
data KalkulatorReady = KalkulatorReady

instance ToHtml KalkulatorReady where
  toHtml = toHtml . toHtmlRaw
  toHtmlRaw KalkulatorReady = template do
    h1_ "Priskalkulator for garn"
    h4_ "Legg inn garn fra oppskriften (originalgarnet)"
    p_ "Maks to typer garn. Hvis det bare én type garn, la 'garn 2'-feltet stå tomt, skriv 0 i antall nøster."
    yarnInput

-- | Input skjema for garndata. Brukes i KalkulatorReady og KalkulatorResultat. Sender til KalkulatorResultat via postKalkulatorResult.
yarnInput :: Monad m => HtmlT m ()
yarnInput = 
    form_ [ method_ "post", action_ "/kalkulator" ] $ do
    -- Venstre
    div_ $ do
      h4_ "Originalgarn"
      input_ [ type_ "text", name_ "yarn1", placeholder_ "Garn 1" ]
      input_ [ type_ "number", name_ "amount1", placeholder_ "Antall nøster" ]
      br_ []
      input_ [ type_ "text", name_ "yarn2", placeholder_ "Garn 2" ]
      input_ [ type_ "number", name_ "amount2", placeholder_ "Antall nøster" ]

    -- Høyre
    div_ $ do
      h4_ "Nytt garn"
      input_ [ type_ "text", name_ "newYarn1", placeholder_ "Nytt garn 1" ]
      input_ [ type_ "text", name_ "newYarn2", placeholder_ "Nytt garn 2" ]

    button_ [ type_ "submit" ] "Regn ut"

data KalkulatorInput = KalkulatorInput
  { yarn1 :: Text,
    amount1 :: Int,
    yarn2 :: Text,
    amount2 :: Maybe Int, --funker ikke som jeg vil
    newYarn1 :: Text,
    newYarn2 :: Text
  } deriving (Show, Generic, FromForm)
  
-- | Bruker data fra input til å regne ut hvor mye som trengs av erstatningsgarnet. Viser det med HTML.
data KalkulatorResultat = KalkulatorResultat
  { yas1 :: [YarnAmount],
    yas2 :: [YarnAmount]
  } deriving (Show, Generic)

instance ToHtml KalkulatorResultat where
  toHtml = toHtml . toHtmlRaw
  toHtmlRaw (KalkulatorResultat ogYarn newYarn) = template do
    let finalYarn = skeinsRequired ogYarn newYarn

    h1_ "Resultat"
    p_ $ do
        "Originalgarn: "
        toHtml ogYarn
    p_ $ do
        "Originalpris (totalt): "
        toHtml $ show $ totalCostTheseYarns ogYarn
        " kr."
    p_ $ do
        "Garn å bruke i stedet: "
        toHtml finalYarn
    p_ $ do
        "Ny pris (totalt): "
        toHtml $ show $ totalCostTheseYarns finalYarn
        " kr."

    br_ []
    br_ []
    h4_ "Prøv på nytt"
    yarnInput


-- | Legg til css styling og metadata. Skal brukes på alle sider.
template :: (Monad m) => HtmlT m a -> HtmlT m a
template body = do
  doctype_
  html_ do
    head_ do
      meta_ [name_ "viewport", content_ "initial-scale=1,width=device-width"]
      title_ "Strikkekalkulator"
      link_ [rel_ "stylesheet", href_ "/static/style.css"]
    body_ body
