-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Route: @/invoice/@
module Lightning.Node.Api.Invoice
  ( InvoiceLabel (..)
  , InvoiceRep (..)
  , InvoiceReq (..)
  , ListInvoiceRep (..)
  , ListInvoice (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData, toQueryParam)

import Lightning (Bolt11, MilliSatoshi, Status)
import Lightning.Node.Api.Json (lightningOptions)


-- | Request to create an invoice.
data InvoiceReq = InvoiceReq
  { irqAmount :: MilliSatoshi
  , irqLabel :: Text
  , irqDescription :: Text
  , irqExpire :: Maybe Int  -- ^ Expire after (seconds).
  , irqPrivate :: Maybe Bool
  }
  deriving (Generic, Show)

newtype InvoiceLabel = InvoiceLabel
  { ilLabel :: Text
  } deriving (Eq, Generic,  Ord, Show)

instance ToJSON InvoiceReq where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON InvoiceReq where
  parseJSON = genericParseJSON lightningOptions

instance ToJSON InvoiceLabel where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON InvoiceLabel where
  parseJSON = genericParseJSON lightningOptions

instance ToHttpApiData InvoiceLabel where
  toQueryParam (InvoiceLabel label) = label

-- | Reply with a created invoice.
data InvoiceRep = InvoiceRep
  { irpPaymentHash :: Text
  , irpExpiresAt :: POSIXTime
  , irpBolt11 :: Bolt11
  }
  deriving (Generic, Show)

instance ToJSON InvoiceRep where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON InvoiceRep where
  parseJSON = genericParseJSON lightningOptions


data ListInvoice = ListInvoice
  { lirLabel :: Text
  , lirPaymentHash :: Text
  , lirExpiresAt :: POSIXTime
  , lirStatus :: Status
  , lirPaymentPreimage :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON ListInvoice where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON ListInvoice where
  parseJSON = genericParseJSON lightningOptions


data ListInvoiceRep = ListInvoiceRep
  { lirInvoices :: [ListInvoice]
  } deriving (Generic, Show)

instance ToJSON ListInvoiceRep where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON ListInvoiceRep where
  parseJSON = genericParseJSON lightningOptions
