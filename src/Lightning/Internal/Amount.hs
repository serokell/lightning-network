-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Lightning.Internal.Amount
  ( MilliSatoshi (..)
  , Satoshi (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
import GHC.Generics (Generic)


newtype MilliSatoshi = MilliSatoshi { msat :: Word64 }
  deriving (Eq, Ord, FromJSON, Generic, Num, Enum, Real, Integral, Show, ToJSON)


newtype Satoshi = Satoshi { sat :: Word64 }
  deriving (Eq, Ord, FromJSON, Generic, Num, Enum, Real, Integral, Show, ToJSON)

-- | Convert 'Satoshi' to 'MilliSatoshi'.
--
-- Note that, in theory, this can overflow, as both are stored in the integer
-- types of the same size.
toMilliSatoshi :: Satoshi -> MilliSatoshi
toMilliSatoshi (Satoshi sat) = MilliSatoshi $ sat * 1000
