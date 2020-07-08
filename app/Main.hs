-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API.Generic (fromServant)
import Servant.Client (BaseUrl (BaseUrl), ClientM, Scheme (Http), client, runClientM, mkClientEnv)
import Servant.Client.Generic (AsClientT, genericClient)
import System.Environment (getArgs)
import System.Random (randomRIO)

import qualified Data.Text as T

import Lightning.Node.Api (Api (_v1), ApiV1 (..))

import qualified Authorization.Macaroon as Mac
import qualified Lightning.Node.Api as L


main :: IO ()
main = do
    [macPath] <- getArgs

    macaroon <- Mac.load macPath

    let
      _getInfo :: ClientM L.NodeInfo
      _genInvoice :: L.InvoiceReq -> ClientM L.InvoiceRep
      _listInvoices :: Maybe L.InvoiceLabel -> ClientM L.ListInvoicesRep
      _listChannels :: ClientM [L.ListChannelsElem]
      --_pay :: L.PayReq -> ClientM L.PayRep

      api :: Api (AsClientT ClientM)
      api = genericClient
      ApiV1
        { _getInfo
        , _genInvoice
        , _listInvoices
        , _listChannels
        --, _pay
        } = fromServant @_ @(AsClientT ClientM) (_v1 api macaroon)

    randomSeq <- randomRIO (1, 99999999999) :: IO Int
    let
      payId = "ct-" <> T.pack (show randomSeq)
      req = L.InvoiceReq 100 (L.InvoiceLabel payId) "client test" Nothing Nothing

    manager <- newManager defaultManagerSettings
    let env = mkClientEnv manager (BaseUrl Http "localhost" 3001 "")

    putStrLn "*** Info ***"
    runClientM _getInfo env >>= print
    putStrLn "*** Generating an invoice ***"
    invoice <- runClientM (_genInvoice req) env
    putStrLn $ "Invoice: " <> show invoice
    putStrLn "*** Listing the invoice ***"
    runClientM (_listInvoices (Just $ L.InvoiceLabel payId)) env >>= print
    putStrLn "*** Listing channels ***"
    runClientM _listChannels env >>= print
