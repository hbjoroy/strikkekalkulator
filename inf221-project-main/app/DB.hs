{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, BlockArguments, DataKinds #-}

module DB where

import Database.Selda as S
import Database.Selda.SQLite
import Data.Maybe (listToMaybe)

data Yarn = Yarn
    {
        name :: Text,
        meterage :: Int,
        price :: Int,
        strikkefasthet :: Int
    } deriving (Show, Generic)
instance SqlRow Yarn

-- | Garntype og tilhørende antall garnnøster
type YarnAmount = (Yarn, Int)


-- | Gjør databasesøk, returnerer Yarn så lenge garnet finnes i databasen. Retunerer nothing hvis ikke. Feilmelding skjer ikke herfra.
getInfoFromName :: Text -> IO (Maybe Yarn) --Maybe i tilfelle garnet ikke finnes i databasen
getInfoFromName n =
  withSQLite "/home/siren/inf221/inf221-project/allyarns.sqlite" $ do
    results <- S.query $ do
        yarn <- select allYarns
        restrict (S.toLower (yarn ! #name) .== S.toLower (literal n))
        pure yarn
    pure (listToMaybe results)

-- | tar en liste med garntyper og antall garn for hver garntype, 
-- regner ut total pris
totalCostTheseYarns :: [YarnAmount] -> Int
totalCostTheseYarns = sum . map (\(yarn, amount) -> price yarn * amount)

-- | Hjelpemetode for totalMeterageNeeded og skeinsRequired, regner ut hvor mange meter totalt det blir med det antallet av det garnet
meterageFor :: YarnAmount -> Int
meterageFor (yarn, amount) = meterage yarn * amount

-- | Hjelpemetode for skeinsRequired. Velger den minste lengden for antall garn fra listen. (Sidene begge garnene brukes samtidig trenger vi bare å vite den korteste.)
totalMeterageNeeded :: [YarnAmount] -> Int
totalMeterageNeeded ys =
    minimum (map meterageFor ys)

-- | Tar inn originalgarn og erstatningsgarn. Regner ut hvor mange nøster som trengs av hver type erstatningsgarn. Returnerer en ny YarnAmountList med resulatene.
skeinsRequired :: [YarnAmount] -> [YarnAmount] -> [YarnAmount]
skeinsRequired oldY newY =
    let m = totalMeterageNeeded oldY
    in map (convert m) newY where
        convert :: Int -> YarnAmount -> YarnAmount
        convert m (nyarn, _) =
            let mps = meterage nyarn
                needed = div (m + mps - 1) mps --same as ceiling ( m / mps)
            in (nyarn, needed)
  
------------------------------------------------------------------------
-- Database oppsett
------------------------------------------------------------------------

-- | allyarns.sqlite
allYarns :: Table Yarn
allYarns = table "allYarns" [#name :- primary]

-- | Bruker tryCreateTable og tryInsert til å opprette tabellen
-- hvis allYarns ikke allerede er laget
initializeTable :: SeldaT SQLite IO Bool
initializeTable = do
    tryCreateTable allYarns
    tryInsert allYarns [ 
        Yarn "Alpakka"                  110 75  22,
        Yarn "Alpakka Følgetråd"        400 75  32,
        Yarn "Alpakka Silke"            200 115 28,
        Yarn "Alpakka Ull"              100 75  21,
        Yarn "Atlas"                    110 55  18,
        Yarn "Babyull Lanett"           175 75  30,
        Yarn "Ballerina Chunky Mohair"  135 135 15,
        Yarn "Børstet Alpakka"          110 95  16,
        Yarn "Cashmere"                 116 249 22,
        Yarn "Double Sunday"            108 75  21,
        Yarn "Double Sunday PK"         108 75  21,
        Yarn "Duo"                      124 75  22,
        Yarn "Fritidsgarn"              70  55  15,
        Yarn "Kos"                      90  109 16,
        Yarn "Line"                     110 55  20,
        Yarn "Mandarin Petit"           180 55  27,
        Yarn "Merinoull"                105 79  22,
        Yarn "Paljett"                  110 209 28,
        Yarn "Peer Gynt"                91  59  22,
        Yarn "Poppy"                    80  169 16,
        Yarn "Sisu"                     175 55  27,
        Yarn "Sunday"                   235 75  27,
        Yarn "Tweed Recycled"           175 159 22,
        Yarn "Tynn Line"                220 55  28,
        Yarn "Tynn Peer Gynt"           205 59  27,
        Yarn "Tynn Silk Mohair"         212 105 24]
