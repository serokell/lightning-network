-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Route: @/getinfo/@
module Lightning.Node.Api.GetInfo
  ( Address (..)
  , NodeInfo (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Lightning.Node.Api.Json (lightningOptions)


-- | A bind address.
data Address = Address
  { aAddress :: Text
  , aPort :: Int
  }
  deriving (Eq, Generic, Show)

instance ToJSON Address where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON Address where
  parseJSON = genericParseJSON lightningOptions


-- | Information about the node.
data NodeInfo = NodeInfo
  { niId :: Text
  , niAlias :: Text
  , niNumPendingChannels :: Int
  , niNumActiveChannels :: Int
  , niNumInactiveChannels :: Int
  , niNumPeers :: Int
  , niTestnet :: Maybe Bool
  , niVersion :: Text
  , niAddress :: [Address]
  , niBinding :: [Address]
  }
  deriving (Eq, Generic, Show)

instance ToJSON NodeInfo where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON NodeInfo where
  parseJSON = genericParseJSON lightningOptions