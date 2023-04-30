{-# language DataKinds, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, PolyKinds, TemplateHaskell, RankNTypes #-}
module Clckwrks.Agreements.API where

import Control.Monad.State (MonadState, get)
import Clckwrks
import Clckwrks.Agreements.Acid as Acid (AgreementsState, GetAgreement(..), GetAgreements(..), GetLatestAgreementsMeta(..), NewAgreement(..), SetAgreements(..), GetAgreeds(..), GetAgreedsByUserId(..), RecordAgreeds(..), UpdateAgreementBody(..))
import Clckwrks.Agreements.Monad (AgreementsM)
import Clckwrks.Agreements.Types (Agreed(..), AgreementMeta(..), AgreementsSettings(..), NewAgreementData(..), AgreementId(..), RevisionId(..), UpdateAgreementData(..), agreementRevision)
import Clckwrks.Agreements.URL as URL (WithURL(..), AgreementsAdminApiURL(..), RequestData, ResponseData)
import Clckwrks.Authenticate.Plugin (getUserId)
import Clckwrks.Unauthorized        (unauthorizedPage)
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
import Data.Proxy (Proxy(..))
import qualified Data.Text          as Text
import qualified Data.Text.Lazy     as TL
import Data.Time.Clock (getCurrentTime)
import Data.Typeable        (Typeable)
import Data.SafeCopy (SafeCopy, safeGet, safePut)
import Data.Serialize (runGetLazy, runPut)
import qualified Data.Text as Text
import Happstack.Authenticate.Core (Email(..))
import Happstack.Server (Request(rqBody), Response, RqBody(..), internalServerError)
import Network.HTTP.Client (responseBody)
import qualified Network.HTTP.Client as HC
import Web.Plugins.Core --             (Plugin(..), Plugins(..), PluginsState(pluginsConfig), When(..), addCleanup, addHandler, addPluginState, addPostHook, initPlugin, getConfig, getPluginRouteFn)

data AgreementsPagePaths = AgreementsPagePaths
  { _agreementsSettingsPath     :: Maybe FilePath
  , _agreementsSignupPluginPath :: Maybe FilePath
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)
makeLenses ''AgreementsPagePaths

emptyAgreementsPagePaths :: AgreementsPagePaths
emptyAgreementsPagePaths = AgreementsPagePaths
  { _agreementsSettingsPath     = Nothing
  , _agreementsSignupPluginPath = Nothing
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
             update (Acid.SetAgreements ma)
             -- liftIO $ putStrLn $ "setAgreementsBaseUrl - updated baseURL to " ++ show mbu
             ok (toResponse (runPut (safePut ())))

createAgreement :: AgreementsM Response
createAgreement =
  do method POST

     mUid <- getUserId
     case mUid of
       Nothing -> unauthorizedPage ("Unable to find a user id associated with this request." :: TL.Text)
       (Just uid) ->
         do mBody <- takeRequestBody =<< askRq
            -- decodeBody (defaultBodyPolicy "/tmp/" 1024 1024 1024)

            -- liftIO $ putStrLn $ "setAgreementsBaseUrl mBody = " ++ show mBody
            case mBody of
              Nothing ->
                badRequest $ toResponse ("missing NewAgreementData value" :: String)
              (Just (Body bd)) ->
                case runGetLazy safeGet bd of
                  (Left e) -> badRequest $ toResponse $ "could not decode NewAgreementData, error = " ++ e
                  (Right (NewAgreementData nm nt bd)) ->
                      do now       <- liftIO $ getCurrentTime
                         agreement <- update (NewAgreement nm now uid nt bd)
                         ok (toResponse (runPut (safePut (agreement ^. agreementRevision))))

updateAgreement :: AgreementsM Response
updateAgreement =
  do method POST

     mUid <- getUserId
     case mUid of
       Nothing -> unauthorizedPage ("Unable to find a user id associated with this request." :: TL.Text)
       (Just uid) ->
         do mBody <- takeRequestBody =<< askRq
            -- decodeBody (defaultBodyPolicy "/tmp/" 1024 1024 1024)

            -- liftIO $ putStrLn $ "setAgreementsBaseUrl mBody = " ++ show mBody
            case mBody of
              Nothing ->
                badRequest $ toResponse ("missing UpdateAgreementData value" :: String)
              (Just (Body bd)) ->
                case runGetLazy safeGet bd of
                  (Left e) -> badRequest $ toResponse $ "could not decode UpdateAgreementData, error = " ++ e
                  (Right (UpdateAgreementData aid nt bd)) ->
                      do now       <- liftIO $ getCurrentTime
                         mAgreement <- update (UpdateAgreementBody aid now uid nt bd)
                         case mAgreement of
                           (Right agreement) ->
                             ok (toResponse (runPut (safePut (agreement ^. agreementRevision))))
                           (Left err) ->
                             badRequest $ toResponse err

getLatestAgreementsMeta :: AgreementsM Response
getLatestAgreementsMeta =
  do method GET
     ams <- query Acid.GetLatestAgreementsMeta
     let serialized = runPut (safePut ams)
     ok $ toResponse serialized

getRequiredAgreements :: AgreementsM Response
getRequiredAgreements =
  do method GET
     ams <- query Acid.GetLatestAgreementsMeta
     let serialized = runPut (safePut ams)
     ok $ toResponse serialized
       where
         removeNote :: AgreementMeta -> AgreementMeta
         removeNote am = am { _amRevisionNote = "" }

handleRequestWithBody :: (SafeCopy (RequestData c), SafeCopy (ResponseData c), WithURL c) => (Proxy c) -> Method -> (RequestData c -> AgreementsM (ResponseData c)) -> AgreementsM Response
handleRequestWithBody _ mtd f =
  do method mtd
     mBody <- takeRequestBody =<< askRq
     case mBody of
       Nothing ->
         badRequest $ toResponse ("Could not find a request body" :: LBS.ByteString)
       (Just (Body bdy)) ->
         case runGetLazy safeGet bdy of
           (Left e) -> badRequest $ toResponse $ "could not decode request body, error = " ++ e
           (Right rq) ->
             do r <- f rq
                ok $ toResponse (runPut (safePut r))

handleRequest :: (SafeCopy (RequestData c), SafeCopy (ResponseData c), WithURL c) => (Proxy (c :: k)) -> Method -> AgreementsM (Either Text.Text (ResponseData c)) -> AgreementsM Response
handleRequest _ mtd f =
  do method mtd
     er <- f
     case er of
       (Left e)  -> badRequest $ toResponse e
       (Right r) -> ok $ toResponse (runPut (safePut r))

getAgreement :: AgreementId -> AgreementsM Response
getAgreement aid =
  handleRequest (Proxy :: Proxy URL.GetAgreement) GET $
    do query (Acid.GetAgreement aid Nothing)

getAgreementRevision :: AgreementId -> RevisionId -> AgreementsM Response
getAgreementRevision aid rid =
  handleRequest (Proxy :: Proxy URL.GetAgreement) GET $
    do query (Acid.GetAgreement aid (Just rid))

-- * Agreed

recordAgreed :: AgreementsM Response
recordAgreed =
  do method POST

     mUid <- getUserId
     case mUid of
       Nothing -> unauthorizedPage ("Unable to find a user id associated with this request." :: TL.Text)
       (Just uid) ->
         do mBody <- takeRequestBody =<< askRq
            -- decodeBody (defaultBodyPolicy "/tmp/" 1024 1024 1024)

            -- liftIO $ putStrLn $ "setAgreementsBaseUrl mBody = " ++ show mBody
            case mBody of
              Nothing ->
                badRequest $ toResponse ("missing [AgreementRevision] value" :: String)
              (Just (Body bd)) ->
                case runGetLazy safeGet bd of
                  (Left e) -> badRequest $ toResponse $ "could not decode [AgreementRevision], error = " ++ e
                  (Right ars) ->
                      do now       <- liftIO $ getCurrentTime
                         let ags = map (\ar -> Agreed { _agreedBy = uid
                                                      , _agreedTo = ar
                                                      , _agreedOn = now
                                                      }) ars
                         update (RecordAgreeds ags)
                         ok (toResponse (runPut (safePut ())))
