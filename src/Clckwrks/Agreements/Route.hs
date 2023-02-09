{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Agreements.Route where

import Clckwrks.Monad               (ClckT, plugins)
import Clckwrks.Admin.Template      (template)
import Clckwrks.Agreements.API       (AgreementsPagePaths(..), AgreementsPluginState(..), createAgreement, getAgreement, {- getAgreementsSettings, -} setAgreements, getLatestAgreementsMeta)
import Clckwrks.Agreements.Monad     (AgreementsConfig(..), AgreementsM, clckT2AgreementsT)
import Clckwrks.Agreements.Types     (agreementsPluginName)
import Clckwrks.Agreements.URL       (AgreementsURL(..), AgreementsAdminURL(..), AgreementsAdminApiURL(..))
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

routeAgreementsAdminAPI :: AgreementsAdminApiURL
               -> AgreementsM Response
routeAgreementsAdminAPI aaURL =
  do liftIO $ putStrLn $ "aaURL = " ++ show aaURL
     case aaURL of
       GetLatestAgreementsMeta -> getLatestAgreementsMeta
       CreateAgreement -> createAgreement
       (GetAgreement aid) -> getAgreement aid
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
       (AgreementsAdmin _) -> requiresRole (Set.fromList [Administrator]) url


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

