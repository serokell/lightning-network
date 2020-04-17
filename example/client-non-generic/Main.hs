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
_pay :: L.PayReq -> ClientM L.PayRep

_getInfo :<|> _genInvoice :<|> _pay = api macaroon


main :: IO ()
main = pure ()
