{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Main where

import Database.Selda
import Database.Selda.SQLite
import DB (initializeTable)
import Data.Proxy
import API
import Network.Wai.Handler.Warp
import Servant
import Server



main :: IO ()
main = withSQLite "/home/siren/inf221/inf221-project/allyarns.sqlite" $ do
    _ <- initializeTable

    liftIO $ do
        ctx <- newApiCtx
        run 8080 (serve (Proxy @API) (hoistServer (Proxy @API) (runApiM ctx) server))











