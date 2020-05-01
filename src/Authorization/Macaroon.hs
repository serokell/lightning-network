-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Lightning REST API macaroon-based authorization.
module Authorization.Macaroon
  ( Macaroon
  , load
  ) where


import Data.ByteString (ByteString)
import Data.Kind (Constraint)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence ((<|))
import Servant.API ((:>))
import Servant.Auth (Auth)
import Servant.Client.Core (Client, HasClient (..), RequestF (requestHeaders))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64


newtype Macaroon = Macaroon ByteString

load :: FilePath -> IO Macaroon
load = (Macaroon <$>) . BS.readFile


-- * Servant Auth

type family HasMacaroon xs :: Constraint where
  HasMacaroon (Macaroon ': xs) = ()
  HasMacaroon (x ': xs) = HasMacaroon xs
  HasMacaroon '[] = MacaroonAuthNotEnabled

class MacaroonAuthNotEnabled

instance
  (HasMacaroon auths, HasClient m api)
  => HasClient m (Auth auths a :> api) where
    type Client m (Auth auths a :> api) = Macaroon -> Client m api

    clientWithRoute m _ req (Macaroon bytes) =
      clientWithRoute m (Proxy :: Proxy api) $
        req
          { requestHeaders =
              ("macaroon", B64.encode bytes) <| requestHeaders req
          }

#if MIN_VERSION_servant_client_core(0,14,0)
    hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy api) nt . cl
#endif
