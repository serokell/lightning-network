<!--
SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>

SPDX-License-Identifier: MPL-2.0
-->

# Changelog

## Unreleased

### Added

- c-lightning REST API v1:
  - `/invoice/listInvoices`
  - `/channel/listChannels`

### Changed

- `InvoiceReq` now contains `InvoiceLabel` instead of `Text`

## 0.0.0.1

### Added

- Lightning:
  - `MilliSatoshi`
  - `Invoice`
  - `Bolt11`
- Macaroon authorization:
  - `Macaroon` type
  - `Macaroon.load`
  - `Macarroon` Auth type for `servant-auth-client`
- c-lightning REST API v1:
  - `/getinfo`
  - `/invoice/genInvoice`
  - `/pay`
