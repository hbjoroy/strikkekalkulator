{-# LANGUAGE OverloadedStrings, BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Server where

import Servant
import API
import Pages
import DB 
import Data.Text
import Control.Monad.IO.Class
import Static (staticDir)
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Control.Monad.Reader (ReaderT, runReaderT)

-- | Hjemmesiden vises på addresse "/". Inneholder linker til de andre sidene.
getHomePage :: ApiM HomePage
getHomePage = pure HomePage

-- | Henter garnet fra databasen hvis det finnes, 404 hvis ikke. ToHtml instance viser garninformasjonen på "/yarn/name"
getYarn :: Text -> ApiM Yarn
getYarn n = do
    rows <- liftIO $ getInfoFromName n
    case rows of
        Just y    -> pure y
        Nothing   -> throwError err404

--------------------------------------------------------
-- STM greier, hermet etter eksempel fra undervisningen
--------------------------------------------------------

-- | Gjør stylesheet på "/static" tilgjengelig. Må oppdateres med cabal 
-- clean cabal build hvis endringer i stylesheet skal bli oppdaget. 
-- Lånt rett fra note-share eksempelet i INF221.
staticH = serveDirectoryEmbedded staticDir

data ApiCtx = ApiCtx
  { originalYarns :: TVar [YarnAmount]
  , chosenYarns :: TVar [YarnAmount]
  }

type ApiM = ReaderT ApiCtx Handler

runApiM :: ApiCtx -> (forall a. ApiM a -> Handler a)
runApiM ctx act = runReaderT act ctx

newApiCtx :: IO ApiCtx
newApiCtx =
  ApiCtx
    <$> STM.newTVarIO []
    <*> STM.newTVarIO []

--------------------------------------------------------
-- Garnbyttekalkulator
--------------------------------------------------------

-- | Siden med skjema for originalgarn og garn det skal erstattes med, submit-knapp sender videre til postKalkulatorResult
getKalkulatorReady :: ApiM KalkulatorReady
getKalkulatorReady = pure KalkulatorReady

-- | Tar input fra skjema, gjør det om til pure og sender det videre til KalkulatorResultat. Utregningen skjer i ToHtml instansen.
postKalkulatorResult :: KalkulatorInput -> ApiM KalkulatorResultat
postKalkulatorResult (KalkulatorInput y1 n1 y2 mn2 newy1 newy2) = do
  -----------------------------------------
  -- det originale garnet
  -----------------------------------------
  --Hente garnet fra databasen, 404 hvis det er feil
  my1 <- liftIO $ getInfoFromName y1
  y1' <- case my1 of
           Nothing  -> throwError err404
           Just val -> pure val

  --Nothing hvis input er tom eller 0, ellers prøve å hente garnet
  my2 <-
    case (y2, mn2) of
      ("", _)        -> pure Nothing
      (_, Nothing)   -> pure Nothing
      (_, Just _)    -> liftIO $ getInfoFromName y2

  y2' <- case (y2, mn2, my2) of
           ("", _, _)           -> pure Nothing
           (_, Nothing, _)      -> pure Nothing
           (_, Just _, Nothing) -> throwError err404
           (_, Just n2, Just y) -> pure (Just (y, n2))

  let original =
        case y2' of
          Nothing        -> [(y1', n1)]
          Just (y,n2)    -> [(y1', n1), (y, n2)]

  -----------------------------------------
  -- det nye garnet, gjøre det samme igjen
  -----------------------------------------
  myNew1 <- liftIO $ getInfoFromName newy1
  new1' <- case myNew1 of
             Nothing  -> throwError err404
             Just val -> pure val

  myNew2 <- do
    case newy2 of
      "" -> pure Nothing
      _  -> liftIO $ getInfoFromName newy2

  new2' <- case (newy2, myNew2) of
            ("", _)        -> pure Nothing
            (_, Nothing)   -> throwError err404
            (_, Just y)    -> pure (Just (y, 0 :: Integer))

  let newBase = [(new1', 0)]
  let new = maybe newBase (\(y,_) -> newBase ++ [(y, 0)]) new2'

  -- sende begge listene videre til KalkulatorResultat
  pure (KalkulatorResultat original new)

server :: ServerT API ApiM
server = getHomePage 
    :<|> getYarn 
    :<|> (getKalkulatorReady
      :<|> postKalkulatorResult)
    :<|> staticH
