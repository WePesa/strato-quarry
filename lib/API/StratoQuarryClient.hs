{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module API.StratoQuarryClient (
   quarryStatusRoute,
   QuarryStatus(..)
  ) where

import API.StratoQuarry
import Control.Monad.Trans.Either

import Servant.Client
 
quarryStatusRoute :: BaseUrl -> EitherT ServantError IO QuarryStatus
quarryStatusRoute = client statusAPI
