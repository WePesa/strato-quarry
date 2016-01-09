{-# LANGUAGE TemplateHaskell #-}
module Blockchain.Quarry.Flags where

import HFlags

defineFlag "debug" False "Show debug output: display info on blocks constructed"
defineFlag "useTestnet" False "Change difficulty computation for ethdev testnet"
