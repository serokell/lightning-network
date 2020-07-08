module Lightning.Node.Api.Peer
  ( ListChannelsElem (..)
  ) where


import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Text
import GHC.Generics (Generic)

import Lightning.Node.Api.Json (lightningOptions)

data ListChannelsElem = ListChannelsElem
  { lprId :: Text
  , lprConnected :: Bool
  , lprMsatoshiToUs :: Int
  , lprMsatoshiTotal :: Int
  } deriving (Generic, Show)

instance ToJSON ListChannelsElem where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON ListChannelsElem where
  parseJSON = genericParseJSON lightningOptions
