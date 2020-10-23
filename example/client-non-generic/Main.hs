-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Data.Proxy (Proxy (Proxy))
import Servant.API ((:<|>) ((:<|>)))
import Servant.API.Generic (ToServantApi)
import Servant.Client (Client, ClientM, client)

import Lightning.Node.Api (Api)

import Authorization.Macaroon (Macaroon)
import Lightning.Internal.Invoice (Bolt11)
import qualified Lightning.Node.Api as L


macaroon :: Macaroon
macaroon = undefined

-- !!!
-- !
-- ! If you change something here, also update the instructions in README.md
-- !
-- !!!

api :: Client ClientM (ToServantApi Api)
api = client (Proxy :: Proxy (ToServantApi Api))

_getInfo :: ClientM L.NodeInfo
_genInvoice :: L.InvoiceReq -> ClientM L.InvoiceRep
_listInvoices :: Maybe L.InvoiceLabel -> ClientM L.ListInvoicesRep
_listChannels :: ClientM [L.ListChannelsElem]
_pay :: L.PayReq -> ClientM L.PayRep
_decodePay :: Bolt11 -> ClientM L.DecodePayRep
_listPayments :: Maybe Bolt11 -> ClientM L.ListPaymentsRep

_getInfo :<|> _genInvoice :<|> _listInvoices :<|> _listChannels :<|> _pay :<|> _decodePay :<|> _listPayments = api macaroon


main :: IO ()
main = pure ()
