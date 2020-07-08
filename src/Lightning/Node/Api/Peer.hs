module Lightning.Node.Api.Peer
  ( ListPeersRep (..)
  ) where


import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Text
import GHC.Generics (Generic)

import Lightning.Node.Api.Json (lightningOptions)


data ListPeersRep = ListPeersRep
  { lprId :: Text
  , lprConnected :: Bool
  , lprMsatoshiToUs :: Int
  , lprMsatoshiTotal :: Int
  } deriving (Generic, Show)

instance ToJSON ListPeersRep where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON ListPeersRep where
  parseJSON = genericParseJSON lightningOptions
