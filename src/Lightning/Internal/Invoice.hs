-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Lightning.Internal.Invoice
  ( Bolt11 (..)
  , Invoice (..)
  , Status (..)
  , toBolt11
  , fromBolt11
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, sumEncoding, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Text (Text)
import Data.Char (toLower)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types


import GHC.Generics (Generic)

-- | A Lightning network invoice.
--
-- Currently this is an opaque type suitable for being
-- passed through to/from the node API.
--
-- TODO: Implement bolt11 encoding/decoding.
newtype Invoice = Invoice Bolt11


-- | An invoice encoded at bolt11.
--
-- TODO: Provide FromJSON instance that checks the format.
newtype Bolt11 = Bolt11 Text
  deriving (FromJSON, Show, ToJSON)

-- | Encode an 'Invoice' as bolt11.
--
-- TODO: Actually implement.
toBolt11 :: Invoice -> Bolt11
toBolt11 (Invoice bolt11) = bolt11

-- | Decode an 'Invoice' from bolt11.
--
-- TODO: Actually implement.
fromBolt11 :: Bolt11 -> Invoice
fromBolt11 = Invoice


data Status = Unpaid | Paid | Expired
  deriving (Generic, Show)

deriveJSON (defaultOptions { constructorTagModifier = map toLower }) ''Status



