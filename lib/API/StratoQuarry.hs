{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module API.StratoQuarry (
   stratoQuarryAPIMain
  ) where

import API.Route.Status
import API.Handler.Status

import Servant
import Network.Wai
import Network.Wai.Handler.Warp

app :: Application
app = serve statusAPI statusGet
 
statusAPI :: Proxy StatusAPI
statusAPI = Proxy

stratoQuarryAPIMain :: IO ()
stratoQuarryAPIMain = run 8083 app
