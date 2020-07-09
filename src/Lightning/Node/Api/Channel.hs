module Lightning.Node.Api.Channel
  ( ListChannelsElem (..)
  ) where


import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Text
import GHC.Generics (Generic)

import Lightning (MilliSatoshi, Satoshi)
import Lightning.Node.Api.Json (lightningOptions)

data ListChannelsElem = ListChannelsElem
  { lceId :: Text
  , lceConnected :: Bool
  , lceMsatoshiToUs :: MilliSatoshi
  , lceMsatoshiTotal :: MilliSatoshi
  , lseTheirChannelReserveSatoshis :: Satoshi
  , lceOurChannelReserveSatoshis :: Satoshi
  } deriving (Generic, Show)

instance ToJSON ListChannelsElem where
  toJSON = genericToJSON lightningOptions
  toEncoding = genericToEncoding lightningOptions

instance FromJSON ListChannelsElem where
  parseJSON = genericParseJSON lightningOptions
