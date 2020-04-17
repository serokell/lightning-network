-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | JSON utilities for c-lightning REST API.
module Lightning.Node.Api.Json
  ( lightningOptions
  ) where

import Data.Aeson (Options (..), camelTo2, defaultOptions)
import Data.Char (isUpper)


lightningOptions :: Options
lightningOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . dropWhile (not . isUpper)
  }