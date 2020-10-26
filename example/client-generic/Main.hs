-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Servant.API.Generic (fromServant)
import Servant.Client (Client, ClientM)
import Servant.Client.Generic (AsClientT, genericClient)

import Authorization.Macaroon (Macaroon)
import Lightning.Internal.Invoice (Bolt11)
import Lightning.Node.Api (Api (_v1), ApiV1 (..))

import qualified Lightning.Node.Api as L


macaroon :: Macaroon
macaroon = undefined


-- !!!
-- !
-- ! If you change something here, also update the instructions in README.md
-- !
-- !!!

api :: Api (AsClientT ClientM)
api = genericClient

_getInfo :: ClientM L.NodeInfo
_genInvoice :: L.InvoiceReq -> ClientM L.InvoiceRep
_listInvoices :: Maybe L.InvoiceLabel -> ClientM L.ListInvoicesRep
_listChannels :: ClientM [L.ListChannelsElem]
_pay :: L.PayReq -> ClientM L.PayRep
_decodePay :: Bolt11 -> ClientM L.DecodePayRep

ApiV1
  { _getInfo
  , _genInvoice
  , _listInvoices
  , _listChannels
  , _pay
  , _decodePay
  } = fromServant @_ @(AsClientT ClientM) (_v1 api macaroon)


main :: IO ()
main = pure ()
