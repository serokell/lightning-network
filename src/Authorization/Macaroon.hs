-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Lightning REST API 'Macaroon' authorization.
--
-- Note: to support both 'Macaroon'-based and 'Token'-based authorization, this
-- module exports copies of 'Token' and 'Bearer' from 'Servant.Auth.Client'.
-- This is necessary because there is no way to extend the 'HasClient' instance
-- of 'Servant.Auth.Client' otherwise.
module Authorization.Macaroon
  ( Macaroon
  , load

    -- * Servant Auth Client
  , Token (..)
  , Bearer
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Constraint)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence ((<|))
import Data.String (IsString)
import GHC.Generics (Generic)
import qualified GHC.TypeLits as Lit
import Network.HTTP.Types.Header (Header)
import Servant.API ((:>))
import Servant.Auth (Auth, JWT)
import Servant.Client.Core (Client, HasClient (..), RequestF (requestHeaders))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64


newtype Macaroon = Macaroon ByteString

load :: FilePath -> IO Macaroon
load = (Macaroon <$>) . BS.readFile

--------------------------------------------------------------------------------
-- Servant Auth Client
--------------------------------------------------------------------------------

-- | A simple bearer token.
--
-- Note: this is identical to the one used in 'Servant.Auth.Client' and can be
-- used to replace it.
newtype Token = Token { getToken :: ByteString }
  deriving (Eq, Show, Read, Generic, IsString)

-- * Authentication combinators

-- | A Bearer token in the Authorization header:
--
--    @Authorization: Bearer <token>@
--
-- This can be any token recognized by the server, for example,
-- a JSON Web Token (JWT).
--
-- Note that, since the exact way the token is validated is not specified,
-- this combinator can only be used in the client. The server would not know
-- how to validate it, while the client does not care.
-- If you want to implement Bearer authentication in your server, you have to
-- choose a specific combinator, such as 'JWT'.
--
-- Note: this is identical to the one used in 'Servant.Auth.Client' and can be
-- used to replace it.
data Bearer

--------------------------------------------------------------------------------
-- Servant Auth Client Machinery
--------------------------------------------------------------------------------

instance (HasAuthClient auths, HasClient m api)
  => HasClient m (Auth auths a :> api) where
    type Client m (Auth auths a :> api) = ClientAuthData auths -> Client m api

    clientWithRoute m _ req authData =
      clientWithRoute m (Proxy :: Proxy api) $
        req
          { requestHeaders =
              clientAuthHeader (Proxy @auths) authData <| requestHeaders req
          }

#if MIN_VERSION_servant_client_core(0,14,0)
    hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy api) nt . cl
#endif

-- | Type family that allows to use 'Macaroon' or 'Token' depending on which is
-- found first in the @auth@ list.
type family ClientAuthData xs where
  ClientAuthData (Macaroon ': xs) = Macaroon
  ClientAuthData (Bearer ': xs)   = Token
  ClientAuthData (JWT ': xs)      = Token
  ClientAuthData (x ': xs)        = ClientAuthData xs
  ClientAuthData '[] = Lit.TypeError (Lit.Text "No known client auth header.")

-- | Supporting typeclass to 'HasClient', this allows for the different 'Header'
-- used by 'Macaroon' and 'Token' (with 'ClientAuthData').
class HasAuthClient (auths :: [*]) where
  clientAuthHeader :: Proxy auths -> ClientAuthData auths -> Header

instance {-# OVERLAPPING #-} HasAuthClient (Macaroon ': xs) where
  clientAuthHeader _ (Macaroon bytes) = ("macaroon", B64.encode bytes)

instance {-# OVERLAPPING #-} HasAuthClient (Bearer ': xs) where
  clientAuthHeader _ (Token token) = ("Authorization", "Bearer " <> token)

instance {-# OVERLAPPING #-} HasAuthClient (JWT ': xs) where
  clientAuthHeader _ (Token token) = ("Authorization", "Bearer " <> token)

instance {-# OVERLAPPABLE #-}
  (HasAuthClient xs, ClientAuthData xs ~ ClientAuthData (x ': xs))
  => HasAuthClient (x ': xs) where
  clientAuthHeader _ = clientAuthHeader (Proxy @xs)

