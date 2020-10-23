-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Route: @/pay/decodePay/<invoice>/@
module Lightning.Node.Api.Pay.Decode
  ( DecodePayRep (..)
  , Route (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Text (Text)
import GHC.Generics (Generic)

import Lightning (Bolt11, MilliSatoshi)
import Lightning.Node.Api.Json (lightningOptions)


-- | Reply with a decoded invoice information.
data DecodePayRep = DecodePayRep
  { drpCurrency           :: Text
  , drpCreatedAt          :: POSIXTime
  , drpExpiry             :: Maybe Int -- ^ Expire after (seconds).
  , drpPayee              :: Text
  , drpMsatoshi           :: Maybe MilliSatoshi  -- ^ Amount requested
  , drpDescription        :: Text
  , drpMinFinalCltvExpiry :: Maybe Int
  , drpPaymentSecret      :: Maybe Text
  , dprFeatures           :: Maybe Text
  , dprRoutes             :: Maybe [Route]
  , dprPaymentHash        :: Text
  , drpSignature          :: Text
  }
  deriving (Generic, Show)

instance ToJSON DecodePayRep where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON DecodePayRep where
  parseJSON = genericParseJSON lightningOptions


newtype Route = Route { rtDescription :: Text }
  deriving (Generic, Show)

instance ToJSON Route where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON Route where
  parseJSON = genericParseJSON lightningOptions
