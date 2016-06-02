
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module API.Model.Status where

import Data.Aeson.TH

data QuarryStatus = QuarryStatus {
    quarryMessage :: String,
    quarryTimestamp :: String   -- replace with UTCTime
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''QuarryStatus)
