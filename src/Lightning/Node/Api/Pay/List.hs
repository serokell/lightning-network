-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Route: @/pay/listPayments?invoice=<invoice>@
module Lightning.Node.Api.Pay.List
  ( ListPaymentsRep (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Text (Text)
import GHC.Generics (Generic)

import Lightning (Bolt11, MilliSatoshi)
import Lightning.Node.Api.Json (lightningOptions)
import Lightning.Node.Api.Pay (PayRep)

-- | Reply with a decoded invoice information.
newtype ListPaymentsRep = ListPaymentsRep { lprPayments :: [PayRep] }
  deriving (Generic, Show)

instance ToJSON ListPaymentsRep where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON ListPaymentsRep where
  parseJSON = genericParseJSON lightningOptions
