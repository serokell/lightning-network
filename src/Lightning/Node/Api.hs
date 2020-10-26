-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | c-lightning REST API client
--
-- See <https://github.com/Ride-The-Lightning/c-lightning-REST>.
module Lightning.Node.Api
  ( Api (..)
  , ApiV1 (..)

  , module A
  ) where

import GHC.Generics (Generic)
import Servant.API ((:>), Capture, Get, JSON, Post, QueryParam, ReqBody)
import Servant.API.Generic (ToServantApi, (:-))
import Servant.Auth (Auth)

import Authorization.Macaroon (Macaroon)
import Lightning.Internal.Invoice (Bolt11)
import Lightning.Node.Api.GetInfo as A (Address (..), NodeInfo (..))
import Lightning.Node.Api.Invoice as A (InvoiceLabel(..), InvoiceRep (..), InvoiceReq (..), ListInvoicesRep (..))
import Lightning.Node.Api.Pay as A (PayReq (..), PayRep (..))
import Lightning.Node.Api.Pay.Decode as A (DecodePayRep (..), Route (..))
import Lightning.Node.Api.Channel as A (ListChannelsElem (..))


data ApiV1 route = ApiV1
  { _getInfo :: route
      :- "getinfo"
      :> Get '[JSON] NodeInfo
  , _genInvoice :: route
      :- "invoice"
      :> "genInvoice"
      :> ReqBody '[JSON] InvoiceReq
      :> Post '[JSON] InvoiceRep
  , _listInvoices :: route
      :- "invoice"
      :> "listInvoices"
      :> QueryParam "label" InvoiceLabel
      :> Get '[JSON] ListInvoicesRep
  , _listChannels :: route
      :- "channel"
      :> "listChannels"
      :> Get '[JSON] [ListChannelsElem]
  , _pay :: route
      :- "pay"
      :> ReqBody '[JSON] PayReq
      :> Post '[JSON] PayRep
  , _decodePay :: route
      :- "pay"
      :> "decodePay"
      :> Capture "invoice" Bolt11
      :> Get '[JSON] DecodePayRep
  }
  deriving (Generic)

data Api route = Api
  { _v1 :: route
      :- "v1"
      :> Auth '[Macaroon] ()
      :> ToServantApi ApiV1
  }
  deriving (Generic)
