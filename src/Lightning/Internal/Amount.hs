-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Lightning.Internal.Amount
  ( MilliSatoshi (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
import GHC.Generics (Generic)


newtype MilliSatoshi = MilliSatoshi { msat :: Word64 }
  deriving (Eq, Ord, FromJSON, Generic, Num, Enum, Real, Integral, Show, ToJSON)
