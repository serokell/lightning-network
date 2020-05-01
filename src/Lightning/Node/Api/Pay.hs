-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Route: @/pay/@
module Lightning.Node.Api.Pay
  ( PayReq (..)
  , PayRep (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Text (Text)
import GHC.Generics (Generic)

import Lightning (Bolt11, MilliSatoshi)
import Lightning.Node.Api.Json (lightningOptions)


-- | Request to pay an invoice.
data PayReq = PayReq
  { prqInvoice :: Bolt11
  -- TODO: Optional amount for "any" invoices.
  }
  deriving (Generic, Show)

instance ToJSON PayReq where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON PayReq where
  parseJSON = genericParseJSON lightningOptions


-- | Reply with a payment information.
data PayRep = PayRep
  { prpId :: Int
  , prpPaymentHash :: Text
  , prpDestination :: Text
  , prpMsatoshi :: Maybe MilliSatoshi  -- ^ Amount requested
  , prpMsatoshiSent :: MilliSatoshi
  , prpCreatedAt :: POSIXTime
  , prpStatus :: Text
  , prpPaymentPreimage :: Text
  , prpBolt11 :: Bolt11
  }
  deriving (Generic, Show)

instance ToJSON PayRep where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON PayRep where
  parseJSON = genericParseJSON lightningOptions
