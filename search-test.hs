#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [servant servant-client async text http-client aeson])" -i runhaskell

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Servant.Client.Core.HasClient
import Servant.Types.SourceT (foreach)

import qualified Servant.Client.Streaming as S

type RequiredQueryParam = QueryParam' [Required, Strict]

type DefaultParams a = "services" :> "FindingService" :> "v1"
                                                      :> RequiredQueryParam "SERVICE-VERSION" Text
                                                      :> RequiredQueryParam "SECURITY-APPNAME" Text
                                                      :> RequiredQueryParam "RESPONSE-DATA-FORMAT" Text
                                                      :> RequiredQueryParam "REST-PAYLOAD" Bool
                                                      :> a

type EbayAPI = DefaultParams (RequiredQueryParam "OPERATION-NAME" Text :> RequiredQueryParam "keywords" Text :> Get '[JSON] Value)
          :<|> DefaultParams (RequiredQueryParam "OPERATION-NAME" Text :> Get '[JSON] Value)

ebayAPI :: Proxy EbayAPI
ebayAPI = Proxy

clientM :: Proxy ClientM
clientM = Proxy

type ClientDefaultedParams a = Text -> Text -> Text -> Bool -> a

findItemsByKeywords' :: ClientDefaultedParams (Text -> Text -> ClientM Value)
getVersion' :: ClientDefaultedParams (Text -> ClientM Value)
(findItemsByKeywords' :<|> getVersion') = client ebayAPI

fillInClientDefaultedParams :: ClientDefaultedParams a -> a
fillInClientDefaultedParams request = request "1.0.0" "MyAppID" "JSON" True

data EbayCalls = EbayCalls
               { findItemsByKeywords :: Text -> ClientM Value
               , getVersion :: ClientM Value
               }

buildEbayCalls :: 

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" 8081 "")
  result <- (flip runClientM) clientEnv $ do
    version <- (fillInClientDefaultedParams getVersion) ("getVersion" :: Text)
    return version
  print (result :: Either ClientError Value)
