-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Route: @/invoice/@
module Lightning.Node.Api.Invoice
  ( InvoiceLabel (..)
  , InvoiceRep (..)
  , InvoiceReq (..)
  , InvoiceStatus (..)
  , ListInvoicesElem (..)
  , ListInvoicesRep (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types ((.:), Value (Object), withObject)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData, toQueryParam)

import Lightning (Bolt11, MilliSatoshi)
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

instance FromJSON InvoiceRep where
  parseJSON = genericParseJSON lightningOptions



-- | Information about a /paid/ invoice.
data PaymentDetails = PaymentDetails
  { pdMsatoshiReceived :: MilliSatoshi
  , pdPaidAt :: POSIXTime
  , pdPaymentPreimage :: Text
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON PaymentDetails where
  parseJSON = genericParseJSON lightningOptions


-- | Status of an invoice.
data InvoiceStatus
  = Unpaid
  | Paid PaymentDetails
  | Expired
  deriving (Generic, Show)


-- | Element with information about one invoice.
data ListInvoicesElem = ListInvoicesElem
  { lieLabel :: Text
  , lieBolt11 :: Bolt11
  , liePaymentHash :: Text
  , lieMsatoshi :: MilliSatoshi
  , lieExpiresAt :: POSIXTime
  , lieStatus :: InvoiceStatus
  } deriving (Generic, Show)

-- | Need to write this out manually due to status convertion.
instance FromJSON ListInvoicesElem where
  parseJSON = withObject "ListInvoicesElem" $ \obj -> do
    lieLabel <- obj .: "label"
    lieBolt11 <- obj .: "bolt11"
    liePaymentHash <- obj .: "payment_hash"
    lieMsatoshi <- obj .: "msatoshi"
    lieExpiresAt <- obj .: "expires_at"
    lieStatus <- obj .: "status" >>= \case
      ("unpaid" :: Text) -> pure Unpaid
      ("expired" :: Text) -> pure Expired
      ("paid" :: Text) -> Paid <$> parseJSON (Object obj)
      _ -> fail "Unexpected status"
    pure $ ListInvoicesElem{..}


-- | Wrapper to match the layout of the returned data.
newtype ListInvoicesRep = ListInvoicesRep
  { lirInvoices :: [ListInvoicesElem]
  } deriving (Generic, Show)

instance FromJSON ListInvoicesRep where
  parseJSON = genericParseJSON lightningOptions
