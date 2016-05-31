{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module API.Route.Status where

import API.Model.Status
import Servant

type StatusAPI =  "strato-quarry" :> "v1.1" :> "status" :> Get '[JSON] QuarryStatus
