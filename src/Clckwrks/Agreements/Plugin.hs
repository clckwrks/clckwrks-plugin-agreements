{-# LANGUAGE DeriveDataTypeable, RecordWildCards, FlexibleContexts, Rank2Types, OverloadedStrings, MultiParamTypeClasses, QuasiQuotes #-}
module Clckwrks.Agreements.Plugin where

import qualified Data.ByteString.Lazy.Char8 as BCL
import Clckwrks
import Clckwrks.Monad               (setExtraHeadTags)
import Clckwrks.Plugin              (clckPlugin)
import Clckwrks.Agreements.Acid     (initialAgreementsState)
import Clckwrks.Agreements.API      (AgreementsPluginState(..), emptyAgreementsPagePaths)
import Clckwrks.Agreements.Monad    (AgreementsConfig(..), runAgreementsT)
import Clckwrks.Agreements.Route    (routeAgreements)
import Clckwrks.Agreements.Types    (agreementsPluginName)
import Clckwrks.Agreements.URL      (AgreementsURL(AgreementsAdmin, AgreementsSignupPlugin), AgreementsAdminURL(AgreementsSettings))
import Clckwrks.Authenticate.API    (setSignupPluginURL)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TVar  (TVar, newTVar, readTVar)
import Control.Monad.State          (get)
import Data.Acid                    (AcidState)
import Data.Acid.Advanced           (update', query')
import Data.Acid.Local              (createCheckpointAndClose, openLocalStateFrom,)
import qualified Data.Set           as Set
import Data.Text                    (Text)
import HSP
import Language.Haskell.HSX.QQ      (hsx)
import System.FilePath              ((</>))
import Web.Plugins.Core             (Plugin(..), Plugins(..), PluginsState(pluginsConfig), When(..), addCleanup, addHandler, addPluginState, addPostHook, initPlugin, getConfig, getPluginRouteFn)


agreementsPlugin :: Plugin AgreementsURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ClckPluginsSt
agreementsPlugin = Plugin
    { pluginName           = agreementsPluginName
    , pluginInit           = agreementsInit
    , pluginDepends        = [ pluginName clckPlugin ]
    , pluginToPathSegments = toPathSegments
    , pluginPostHook       = addAgreementsAdminMenu
    }

plugin :: ClckPlugins -- ^ plugins
       -> Text        -- ^ baseURI
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI agreementsPlugin

getPluginsConfig :: (MonadIO m) => Plugins theme n hook config st
             -> m config
getPluginsConfig (Plugins tps) =
    liftIO $ atomically $ pluginsConfig <$> readTVar tps

agreementsInit :: ClckPlugins
              -> IO (Maybe Text)
agreementsInit plugins =
  do ~(Just agreementsShowURL) <- getPluginRouteFn plugins (pluginName agreementsPlugin)

     -- open the Agreements AcidState and add cleanup for it
     mTopDir <- clckTopDir <$> getConfig plugins
     let basePath = maybe "_state" (\td -> td </> "_state") mTopDir -- FIXME
     acid <- openLocalStateFrom (basePath </> "agreements") initialAgreementsState
     addCleanup plugins Always (createCheckpointAndClose acid)

     let agreementsConfig = AgreementsConfig { agreementsState     = acid
                                             -- , pageClckURL   = clckShowFn
                                           }

     addHandler plugins (pluginName agreementsPlugin) (agreementsHandler agreementsShowURL agreementsConfig)
--      addExtraHeadTags plugins (pluginName agreementsPlugin, [[hsx| <script>console.log('agreements init goes here')</script>|]])
     addPluginState plugins (pluginName agreementsPlugin) (AgreementsPluginState { _agreementsPagePaths      = emptyAgreementsPagePaths
                                                                                 , _agreementsAcidState      = acid
                                                                                 })
     setSignupPluginURL plugins (pluginName agreementsPlugin) (agreementsShowURL AgreementsSignupPlugin [])
     pure Nothing

addAgreementsAdminMenu :: ClckT url IO ()
addAgreementsAdminMenu =
    do p <- plugins <$> get
       ~(Just agreementsShowURL) <- getPluginRouteFn p (pluginName agreementsPlugin)
       let agreementsSettingsURL  = agreementsShowURL (AgreementsAdmin AgreementsSettings) []
       addAdminMenu ("Agreements"
                    , [ (Set.fromList [Administrator, Editor], "Agreements"   , agreementsSettingsURL)
                      ]
                    )

agreementsHandler :: (AgreementsURL -> [(Text, Maybe Text)] -> Text)
                 -> AgreementsConfig
                 -> ClckPlugins
                 -> [Text]
                 -> ClckT ClckURL (ServerPartT IO) Response
agreementsHandler showAgreementsURL agreementsConfig plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) ->
          ClckT $ withRouteT flattenURL $ unClckT $ runAgreementsT agreementsConfig $ routeAgreements u
    where
      flattenURL ::   ((url' -> [(Text, Maybe Text)] -> Text) -> (AgreementsURL -> [(Text, Maybe Text)] -> Text))
      flattenURL _ u p = showAgreementsURL u p
