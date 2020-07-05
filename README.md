# Lightning Network (`lightning-network`)

A Haskell interface for the Lightning Network.

Tools for working with payments on the Lightning Netowrk and interacting
with a Lightning Network node through its REST API.


## Use

### Overview

1. Use `Authorization.Macaroon.load` to read a macaroon from a file.
2. Import servant `Api` from `Lightning.Node.Api`.
3. Generate a servant client for this `Api`.
4. Use the client to talk to a node.

### Macaroon

```haskell
import qualified Authorization.Macaroon as Mac
```

And then use `Mac.load` to read your macaroon from a file.

### Generic client

A generic client will give you back a _record_ whose fields will contain
functions for accessing the API.

You will have to add these to your dependencies:

  - `servant`
  - `servant-client`
  - `servant-client-core`

Here is how to get a top-level generic client:

```haskell
import Servant.API.Generic (fromServant)
import Servant.Client (Client, ClientM)
import Servant.Client.Generic (AsClientT, genericClient)

import Lightning.Node.Api (Api (_v1), ApiV1 (..))

import qualified Lightning.Node.Api as L


api :: Api (AsClientT ClientM)
api = genericClient
```

Digging deeper to get actual endpoints is a little tricky (for example,
see [this issue](https://github.com/haskell-servant/servant/issues/1015)).
In order to unwrap the next level of the API, you will already need your
macaroon (see the previous section).

_TODO: Come up with a better way._

```haskell
_getInfo :: ClientM L.NodeInfo
_genInvoice :: L.InvoiceReq -> ClientM L.InvoiceRep
_listInvoices :: Maybe L.InvoiceLabel -> ClientM L.ListInvoicesRep
_pay :: L.PayReq -> ClientM L.PayRep

ApiV1
  { _getInfo
  , _genInvoice
  , _listInvoices
  , _pay
  } = fromServant @_ @(AsClientT ClientM) (_v1 api macaroon)
```

### Non-generic client

Alternatively, you can generate a plain non-generic servant client.

You will have to add these to your dependencies:

  - `servant`
  - `servant-client`

```haskell
import Data.Proxy (Proxy (Proxy))
import Servant.API ((:<|>) ((:<|>)))
import Servant.API.Generic (ToServantApi)
import Servant.Client (Client, ClientM, client)
import Lightning.Node.Api (Api)

import Authorization.Macaroon (Macaroon)
import qualified Lightning.Node.Api as L


api :: Client ClientM (ToServantApi Api)
api = client (Proxy :: Proxy (ToServantApi Api))

_getInfo :: ClientM L.NodeInfo
_genInvoice :: L.InvoiceReq -> ClientM L.InvoiceRep
_listInvoices :: Maybe L.InvoiceLabel -> ClientM L.ListInvoicesRep
_pay :: L.PayReq -> ClientM L.PayRep

_getInfo :<|> _genInvoice :<|> _pay = api macaroon
```


## Contributing

If you encounter any issues when using this library or have improvement ideas,
please open report in issue on GitHub. You are also very welcome to submit
pull request, if you feel like doing so.


## License

[MPL-2.0] © [Serokell]

[MPL-2.0]: https://spdx.org/licenses/MPL-2.0.html
[Serokell]: https://serokell.io/
