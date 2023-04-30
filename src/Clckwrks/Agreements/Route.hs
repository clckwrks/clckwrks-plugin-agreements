{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Agreements.Route where

import Clckwrks.Monad               (ClckT, plugins)
import Clckwrks.Admin.Template      (template)
import Clckwrks.Agreements.API       (AgreementsPagePaths(..), AgreementsPluginState(..), createAgreement, getAgreement, getAgreementRevision, getRequiredAgreements, setAgreements, getLatestAgreementsMeta, recordAgreed, updateAgreement)
import Clckwrks.Agreements.Monad     (AgreementsConfig(..), AgreementsM, clckT2AgreementsT)
import Clckwrks.Agreements.Page.ViewAgreement (viewAgreementPage)
import Clckwrks.Agreements.Types     (agreementsPluginName)
import Clckwrks.Agreements.URL       (AgreementsURL(..), AgreementsAdminURL(..), AgreementsApiURL(..), AgreementsAdminApiURL(..))
import Clckwrks.ProfileData.API     (Role(..), requiresRole_)
import Control.Monad.State          (get)
import Control.Monad.Trans          (liftIO)
import qualified Data.Set           as Set
import Happstack.Server             ( Response, Happstack, Method(GET), escape, methodM, notFound, toResponse
                                    , ok, internalServerError, serveFile, asContentType
                                    )
import HSP.XMLGenerator
import HSP.XML                   (fromStringLit)
import Language.Haskell.HSX.QQ      (hsx)
import Web.Plugins.Core             (getPluginRouteFn, getPluginState)


routeAgreements :: AgreementsURL
               -> AgreementsM Response
routeAgreements url' =
  do url <- checkAuth url'
     case url of
       AgreementsAdmin (AgreementsAdminApi aaURL) -> routeAgreementsAdminAPI aaURL
       AgreementsAdmin AgreementsSettings         -> servePagelet (AgreementsAdmin AgreementsSettingsJs)
       AgreementsAdmin AgreementsSettingsJs       ->
         do p <- plugins <$> get
            ~(Just mps) <- getPluginState p agreementsPluginName
            case _agreementsSettingsPath (_agreementsPagePaths mps) of
              Nothing -> internalServerError $ toResponse ("path to agreements-settings not configure." :: String)
              (Just p) -> do -- liftIO $ putStrLn $ "agreements-settings path is = "++ p
                             serveFile (asContentType "text/javascript;charset=UTF-8") p

       AgreementsRequired ->
         do getRequiredAgreements

       ViewAgreement aid ->
         do viewAgreementPage aid Nothing

       ViewAgreementRevision aid rid ->
         do viewAgreementPage aid (Just rid)

       AgreementsSignupPlugin ->
         do p <- plugins <$> get
            ~(Just mps) <- getPluginState p agreementsPluginName
            case _agreementsSignupPluginPath (_agreementsPagePaths mps) of
              Nothing -> internalServerError $ toResponse ("path to agreements-signup-plugin not configure." :: String)
              (Just p) -> do -- liftIO $ putStrLn $ "agreements-settings path is = "++ p
                             serveFile (asContentType "text/javascript;charset=UTF-8") p
       (AgreementsApi aURL) -> routeAgreementsAPI aURL

routeAgreementsAPI :: AgreementsApiURL
               -> AgreementsM Response
routeAgreementsAPI aURL =
  case aURL of
    RecordAgreed -> recordAgreed

routeAgreementsAdminAPI :: AgreementsAdminApiURL
               -> AgreementsM Response
routeAgreementsAdminAPI aaURL =
  do liftIO $ putStrLn $ "aaURL = " ++ show aaURL
     case aaURL of
       GetLatestAgreementsMeta -> getLatestAgreementsMeta
       CreateAgreement -> createAgreement
       UpdateAgreement -> updateAgreement
       (GetAgreement aid) -> getAgreement aid
       (GetAgreementRevision aid rid) -> getAgreementRevision aid rid

{-
       GetAgreementsSettings ->
         do getAgreementsSettings
-}
       SetAgreements ->
         do setAgreements

checkAuth :: (Happstack m, Monad m) =>
             AgreementsURL
          -> ClckT AgreementsURL m AgreementsURL
checkAuth url =
  do p <- plugins <$> get
     ~(Just clckShowFn) <- getPluginRouteFn p "clck" -- (pluginName clckPlugin) -- a mildly dangerous hack to avoid circular depends
     let requiresRole = requiresRole_ clckShowFn
     case url of
       AgreementsSignupPlugin  -> pure url
       AgreementsRequired      -> pure url
       (ViewAgreement aid)     -> pure url
       (ViewAgreementRevision aid rid) -> pure url
       (AgreementsAdmin (AgreementsAdminApi (GetAgreementRevision {})))    -> pure url
       (AgreementsAdmin _)    -> requiresRole (Set.fromList [Administrator]) url
       (AgreementsApi RecordAgreed) -> pure url


servePagelet :: AgreementsURL -> AgreementsM Response
servePagelet url =
  do p <- plugins <$> get
--     ~(Just mps) <- getPluginState p agreementsPluginName
     ~(Just agreementsShowFn) <- getPluginRouteFn p agreementsPluginName
     let src = agreementsShowFn url []
     template "Agreements" [hsx| <script language="javascript" src=src></script> |] $
        [hsx|
           <div id="pagelet-div"><% show url %></div>
           |]

