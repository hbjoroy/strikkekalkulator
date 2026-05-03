{-# LANGUAGE DataKinds, TypeOperators #-}

module API where

import Servant
import Servant.HTML.Lucid
import DB
import Data.Text

import Pages

type API = Get '[HTML] HomePage
      :<|> "yarn" :> Capture "name" Text :> Get '[HTML] Yarn
      :<|> KalkulatorAPI
      :<|> "static" :> Raw

-- | Faktorere ut "kalkulator", nested API, som i https://docs.servant.dev/en/latest/cookbook/structuring-apis/StructuringApis.html
type KalkulatorAPI =
      "kalkulator" :>
      (
            Get '[HTML] KalkulatorReady
            :<|> ReqBody '[FormUrlEncoded] KalkulatorInput :> Post '[HTML] KalkulatorResultat
      )