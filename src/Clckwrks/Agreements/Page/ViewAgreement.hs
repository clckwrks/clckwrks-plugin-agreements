{-# LANGUAGE GADTs, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Agreements.Page.ViewAgreement where

import Control.Applicative ((<$>))
import Control.Lens         ((^.))
import Clckwrks            (query)
import Clckwrks.Monad      (pcdata, cdata)
import Clckwrks.Monad (ClckT, ThemeStyleId(..), plugins, themeTemplate)
import Clckwrks.URL (ClckURL(..))
import Clckwrks.Agreements.Acid as Acid (GetAgreement(..))
import Clckwrks.Agreements.Monad (AgreementsM)
import Clckwrks.Agreements.Page.Template (template)
import Clckwrks.Agreements.Types (Agreement, AgreementId, RevisionId, agreementName, _revisionBody)
import Clckwrks.Agreements.URL
import Control.Monad.State (get)
import qualified Data.Map as Map
import Data.Text.Lazy (fromStrict)
import Happstack.Server (Response, ServerPartT, internalServerError)
import HSP
import Language.Haskell.HSX.QQ (hsx)

viewAgreementPage :: AgreementId -> Maybe RevisionId -> AgreementsM Response
viewAgreementPage aid rid =
  do ea <- query (Acid.GetAgreement aid rid)
     case ea of
       (Left e) ->
         do internalServerError ()
            template "Agreement" ()
              [hsx|
               <div class="agreements-plugin agreements-view-agreement">
                <p><% e %></p>
               </div>
               |]
       (Right agreement) ->
          --                  <h2><% agreement ^. agreementName %></h2>
         template "Agreement" ()
          [hsx| <div class="agreements-plugin agreements-view-agreement">
                 <% rb agreement %>
                </div>
              |]
    where
      rb :: Agreement -> XML
      rb agreement =
           case Map.lookup "en_US" (_revisionBody agreement) of
             Nothing ->
               pcdata "Body not found."
             (Just b) ->
               cdata (fromStrict b)
{-
  do plugins <- plugins <$> get
     themeTemplate plugins (ThemeStyleId 0) "Agreement" () [hsx|
      <div class="">
        <h2><% agreement ^. agreementName %></h2>
        <% rb %>
      </div> |]
       where
         rb =
           case Map.lookup "en_US" (_revisionBody agreement) of
             Nothing ->
               CData False ""
             (Just b) ->
               CData False b

-}
