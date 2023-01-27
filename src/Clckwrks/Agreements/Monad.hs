{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TypeFamilies, TypeSynonymInstances #-}
module Clckwrks.Agreements.Monad where

import Clckwrks.Acid                 (GetAcidState(..))
import Clckwrks.Monad                (Content(..), ClckT(..), ClckFormT, ClckState(..), ClckPluginsSt(..), mapClckT, runClckT, withRouteClckT, getPreProcessors)
import Control.Monad.Fail            (MonadFail(fail))
import Control.Monad.Reader          (MonadReader(ask,local), ReaderT(runReaderT))
import Control.Monad.State           (StateT, put, get, modify)
import Control.Monad.Trans           (MonadIO(liftIO))
import Clckwrks.URL                  (ClckURL)
import Clckwrks.Agreements.Acid
import Clckwrks.Agreements.Types
import Clckwrks.Agreements.URL
import Clckwrks.Agreements.Types
import Clckwrks.Plugin               (clckPlugin)
import Control.Monad.Trans           (lift)
import Data.Acid                     (AcidState)
import Data.Data                     (Typeable)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import Happstack.Server              (Happstack, Input, ServerPartT)
import Web.Plugins.Core              (Plugin(pluginName), getPluginRouteFn)
import Web.Routes                    (RouteT(..), showURL, withRouteT)

data AgreementsConfig = AgreementsConfig
    { agreementsState        :: AcidState AgreementsState
--    , pageClckURL      :: ClckURL -> [(T.Text, Maybe T.Text)] -> T.Text
    }

type AgreementsT m = ClckT AgreementsURL (ReaderT AgreementsConfig m)
type AgreementsT' url m = ClckT url (ReaderT AgreementsConfig m)
type AgreementsM   = ClckT AgreementsURL (ReaderT AgreementsConfig (ServerPartT IO))
type AgreementsAdminM = ClckT AgreementsAdminURL (ReaderT AgreementsConfig (ServerPartT IO))

-- * Monad instances

instance (Monad m) => MonadReader AgreementsConfig (AgreementsT' url m) where
    ask = ClckT $ ask
    local f (ClckT m) = ClckT $ local f m

instance (Functor m, Monad m) => GetAcidState (AgreementsT' url m) AgreementsState where
    getAcidState =
        agreementsState <$> ask


runAgreementsT :: AgreementsConfig -> AgreementsT m a -> ClckT AgreementsURL m a
runAgreementsT mc m = mapClckT f m
    where
      f r = runReaderT r mc

runAgreementsT'' :: Monad m =>
               (AgreementsURL -> [(T.Text, Maybe T.Text)] -> T.Text)
            -> AgreementsConfig
            -> AgreementsT m a
            -> ClckT url m a
runAgreementsT'' showAgreementsURL stripeConfig m = ClckT $ withRouteT flattenURL $ unClckT $ runAgreementsT stripeConfig $ m
    where
      flattenURL ::   ((url' -> [(T.Text, Maybe T.Text)] -> T.Text) -> (AgreementsURL -> [(T.Text, Maybe T.Text)] -> T.Text))
      flattenURL _ u p = showAgreementsURL u p


-- withRouteClckT ?
flattenURLClckT :: (url1 -> [(T.Text, Maybe T.Text)] -> T.Text)
                -> ClckT url1 m a
                -> ClckT url2 m a
flattenURLClckT showClckURL m = ClckT $ withRouteT flattenURL $ unClckT m
    where
      flattenURL _ = \u p -> showClckURL u p

clckT2AgreementsT :: (Functor m, MonadIO m, MonadFail m, Typeable url1) =>
             ClckT url1 m a
          -> AgreementsT m a
clckT2AgreementsT m =
    do p <- plugins <$> get
       ~(Just clckShowFn) <- getPluginRouteFn p (pluginName clckPlugin)
       flattenURLClckT clckShowFn $ mapClckT addReaderT m
    where
      addReaderT :: (Monad m) => m (a, ClckState) -> ReaderT AgreementsConfig m (a, ClckState)
      addReaderT m =
          do (a, cs) <- lift m
             return (a, cs)
