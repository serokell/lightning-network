-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Route: @/invoice/@
module Lightning.Node.Api.Invoice
  ( InvoiceLabel (..)
  , InvoiceRep (..)
  , InvoiceReq (..)
  , ListInvoicesElem (..)
  , ListInvoicesRep (..)
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


-- | Element with information about one invoice.
data ListInvoicesElem = ListInvoicesElem
  { lieLabel :: Text
  , liePaymentHash :: Text
  , lieExpiresAt :: POSIXTime
  , lieStatus :: Status
  , liePaymentPreimage :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON ListInvoicesElem where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON ListInvoicesElem where
  parseJSON = genericParseJSON lightningOptions


-- | Wrapper to match the layout of the returned data.
newtype ListInvoicesRep = ListInvoicesRep
  { lirInvoices :: [ListInvoicesElem]
  } deriving (Generic, Show)

instance ToJSON ListInvoicesRep where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON ListInvoicesRep where
  parseJSON = genericParseJSON lightningOptions
