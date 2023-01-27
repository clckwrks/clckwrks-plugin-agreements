{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, QuasiQuotes #-}
module Clckwrks.Agreements.Page.Template where

import Clckwrks
import Clckwrks.Agreements.Monad
import Clckwrks.Agreements.URL
import Clckwrks.Plugin
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import HSP hiding (escape)
import Happstack.Server.HSP.HTML ()
import Language.Haskell.HSX.QQ       (hsx)
import Web.Plugins.Core (Plugin(..), getPluginRouteFn, getTheme)

template :: ( EmbedAsChild AgreementsM headers
            , EmbedAsChild AgreementsM body
            ) =>
            Text
         -> headers
         -> body
         -> AgreementsM Response
template ttl hdrs bdy =
    do p <- plugins <$> get
       ~(Just clckShowFn) <- getPluginRouteFn p (pluginName clckPlugin)
       hdrXml <- fmap (map unClckChild) $ unXMLGenT $ asChild hdrs
       bdyXml <- fmap (map unClckChild) $ unXMLGenT $ asChild bdy
       fmap toResponse $ mapClckT f $ ClckT $ withRouteT (\f -> clckShowFn) $ unClckT $ (themeTemplate p (ThemeStyleId 0) ttl hdrXml bdyXml)
    where
      f :: ServerPartT IO (a, ClckState) -> ReaderT AgreementsConfig (ServerPartT IO) (a, ClckState)
      f = lift

-- | create @\<form action=action method=\"POST\" enctype=\"multipart/form-data\"\>@
form :: (XMLGenerator x, StringType x ~ L.Text, EmbedAsAttr x (Attr L.Text action)) =>
        action                  -- ^ action url
     -> [(L.Text,L.Text)]       -- ^ hidden fields to add to form
     -> [XMLGenT x (XMLType x)] -- ^ children
     -> [XMLGenT x (XMLType x)]
form action hidden children =
    [ [hsx| <form action=action method="POST" enctype="multipart/form-data" role="form">
       <% mapM mkHidden hidden %>
       <% children %>
      </form> |]
    ]
    where
      mkHidden (name, value) =
          [hsx| <input type="hidden" name=name value=value /> |]
