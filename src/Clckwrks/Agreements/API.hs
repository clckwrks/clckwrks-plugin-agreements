{-# language DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
module Clckwrks.Agreements.API where

import Control.Monad.State (MonadState, get)
import Clckwrks
import Clckwrks.Agreements.Acid (AgreementsState, GetAgreements(..), GetLatestAgreementsMeta(..), {- GetAgreementsSettings(..), -} SetAgreements(..))
import Clckwrks.Agreements.Monad (AgreementsM)
import Clckwrks.Agreements.Types (AgreementsSettings(..))
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Data.Acid (AcidState)
import qualified Data.Aeson as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Acid as Acid
import qualified Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import Data.Data            (Data)
import qualified Data.Functor
import Data.Maybe (mapMaybe)
import qualified Data.Text
import Data.Typeable        (Typeable)
import Data.SafeCopy (safeGet, safePut)
import Data.Serialize (runGetLazy, runPut)
import qualified Data.Text as Text
import Happstack.Authenticate.Core (Email(..))
import Happstack.Server (Request(rqBody), Response, RqBody(..), internalServerError)
import Network.HTTP.Client (responseBody)
import qualified Network.HTTP.Client as HC
import Web.Plugins.Core --             (Plugin(..), Plugins(..), PluginsState(pluginsConfig), When(..), addCleanup, addHandler, addPluginState, addPostHook, initPlugin, getConfig, getPluginRouteFn)

data AgreementsPagePaths = AgreementsPagePaths
  { _agreementsSettingsPath :: Maybe FilePath
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)
makeLenses ''AgreementsPagePaths

emptyAgreementsPagePaths :: AgreementsPagePaths
emptyAgreementsPagePaths = AgreementsPagePaths
  { _agreementsSettingsPath = Nothing
  }

data AgreementsPluginState = AgreementsPluginState
  { _agreementsPagePaths      :: AgreementsPagePaths
  , _agreementsAcidState      :: AcidState AgreementsState
  }
  deriving Typeable
makeLenses ''AgreementsPluginState
{-
getAgreementsSettings :: AgreementsM Response
getAgreementsSettings =
  do ms <- query GetAgreementsSettings
     -- liftIO $ print ms
     let serialized = runPut (safePut ms)
     -- liftIO $ putStrLn $ show $ serialized
     ok (toResponse serialized)
-}
setAgreements :: AgreementsM Response
setAgreements =
  do rq <- askRq
     -- liftIO $ putStrLn $ "setAgreementsBaseUrl - rqMethod = " ++ show (rqMethod rq)
     method POST

     mBody <- takeRequestBody =<< askRq
     -- decodeBody (defaultBodyPolicy "/tmp/" 1024 1024 1024)

     -- liftIO $ putStrLn $ "setAgreementsBaseUrl mBody = " ++ show mBody
     case mBody of
       Nothing ->
         badRequest $ toResponse ("missing Agreements value" :: String)
       (Just (Body bd)) ->
         case runGetLazy safeGet bd of
           (Left e) -> badRequest $ toResponse $ "could not decode Agreements, error = " ++ e
           (Right ma) -> do
             update (SetAgreements ma)
             -- liftIO $ putStrLn $ "setAgreementsBaseUrl - updated baseURL to " ++ show mbu
             ok (toResponse (runPut (safePut ())))

getLatestAgreementsMeta :: AgreementsM Response
getLatestAgreementsMeta =
  do method GET
     ams <- query GetLatestAgreementsMeta
     let serialized = runPut (safePut ams)
     ok $ toResponse serialized

