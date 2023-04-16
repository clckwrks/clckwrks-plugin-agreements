{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Clckwrks.Agreements.SignupPlugin where

import Control.Lens ((^.))
import Chili.Types (Event(ReadyStateChange), EventObject, JSDocument, JSNode, XMLHttpRequest, addEventListener, appendChild, byteStringToArrayBuffer, createJSElement, createJSTextNode, currentDocument, ev, getChecked, getData, getReadyState, getResponseByteString, getResponseType, getStatus, newXMLHttpRequest, open, parentNode, remove, removeChildren, send, sendArrayBuffer, setAttribute, setTextContent, setRequestHeader, setResponseType, toJSNode, unJSNode)
import Data.Aeson         (Value(..), Object(..), Result(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.JSString as JS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Dominator.DOMC
import Happstack.Authenticate.Core (JSONResponse(..), Status(..))
import Happstack.Authenticate.Client
import Clckwrks.Agreements.Types (AgreementMeta(..), agreementsPluginName, AgreementId(..), RevisionId(..), AgreementRevision, agreementRevision)
import Clckwrks.Agreements.URL (ResponseData, RequestData, AgreementsURL(..), AgreementsAdminURL(..), AgreementsApiURL(..), AgreementsAdminApiURL(..), TaggedURL(..), WithURL(..))
import Data.SafeCopy (SafeCopy, safeGet, safePut)
import Data.Serialize (runGet, runPut)
import Network.HTTP.Types (StdMethod(GET, POST), renderStdMethod)
import Web.Routes (RouteT(..), toPathInfo, toPathSegments)
import Text.Read (readMaybe)

{-
main :: IO ()
main =
  do putStrLn "AgreementsSignupPlugin begin."
     msps' <- getHappstackAuthenticateClientPlugins
     case msps' of
         Nothing -> putStrLn "Could not fetch Signup plugins"
         (Just sps') ->
           do putStrLn "AgreementsSignupPlugins"
              mapM_ (putStrLn . Text.unpack . fst) [("dummy",dummyPlugin)]
     putStrLn "AgreementsSignupPlugin end."

-}

agreementsSignupForm :: JSDocument -> IO (JSNode, [((AgreementId, RevisionId), (Text, Text))] -> IO ())
agreementsSignupForm =
  [domc|
       <div name="agreements-list" data-agreements="{{ agreementList model }}" class="">{{# agreementText model }}</div>
       |]
    where
      renderAgreementMeta :: JSDocument -> (Text, Text) -> IO JSNode
      renderAgreementMeta d (agreementName, agreementURL) =
        do (Just a) <- createJSElement d "a"
           t <- createJSTextNode d agreementName
           href <- setAttribute a "href" agreementURL
           target <- setAttribute a "target" "_blank"
           appendChild a t
           pure (toJSNode a)
      punctuate :: JSDocument -> [JSNode] -> IO [JSNode]
      punctuate d []    = pure []
      punctuate d [a]   =
        do (Just period) <- createJSElement d "span"
           t <- createJSTextNode d "."
           appendChild period t
           pure [a, toJSNode period]
      punctuate d [a,b] =
        do  -- and
           (Just and) <- createJSElement d "span"
           t <- createJSTextNode d " and "
           appendChild and t

           -- period
           (Just period) <- createJSElement d "span"
           t <- createJSTextNode d "."
           appendChild period t

           pure [a, toJSNode and, b, toJSNode period]

      punctuate d [a,b,c] =
        do -- comma
           (Just comma1) <- createJSElement d "span"
           t <- createJSTextNode d ", "
           appendChild comma1 t

           -- comma
           (Just comma2) <- createJSElement d "span"
           t <- createJSTextNode d ", "
           appendChild comma2 t

           -- and
           (Just and) <- createJSElement d "span"
           t <- createJSTextNode d " and "
           appendChild and t

           -- period
           (Just period) <- createJSElement d "span"
           t <- createJSTextNode d "."
           appendChild period t

           pure [a, toJSNode comma1, b, toJSNode comma2, toJSNode and, c, toJSNode period]

      punctuate d (a:as) =
        do -- comma
           (Just comma) <- createJSElement d "span"
           t <- createJSTextNode d ", "
           appendChild comma t
           rest <- punctuate d as
           pure (a : toJSNode comma : rest)

      agreementList :: [((AgreementId, RevisionId), (Text, Text))] -> String
      agreementList ams =
        let ars = map (\((AgreementId a, RevisionId r), _) -> (a,r)) ams
        in show ars

      agreementText :: [((AgreementId, RevisionId), (Text, Text))] -> IO [JSNode]
      agreementText [] = pure []
      agreementText ams =
        do (Just d)    <- currentDocument
           (Just s) <- createJSElement d "span"
           agreementNodes <- mapM (renderAgreementMeta d . snd) ams
           punctuated <- punctuate d agreementNodes
           t <- createJSTextNode d  $ "By clicking sign up, you agree to our "
           appendChild s t
           pure (toJSNode s : punctuated)

agreementsRequiredHandler :: (AgreementsURL -> Text) -> ([((AgreementId, RevisionId), (Text, Text))] -> IO ()) -> XMLHttpRequest -> EventObject ReadyStateChange -> IO ()
agreementsRequiredHandler agreementShowFn update xhr ev =
  do debugStrLn $ "agreementsRequiredHandler - readystatechange"
     status <- getStatus xhr
     rs     <- getReadyState xhr
     case rs of
       4 | status `elem` [200, 201] ->
           do ty  <- getResponseType xhr
              mbs <- getResponseByteString xhr
              case mbs of
                Nothing -> debugStrLn $ "agreementsRequiredHandler - no response body - responseType = " ++ Text.unpack ty
                (Just bs) ->
                  do case runGet safeGet bs of
                       (Left e) ->
                         do debugStrLn $ "agreementsRequiredHandler - failed to decode [AgreementMeta] - " ++ e
                       (Right ams) ->
                         do debugStrLn $ show ams
                            let mkPair :: AgreementMeta -> ((AgreementId, RevisionId), (Text, Text))
                                mkPair am = ((_amAgreementId am, _amRevisionId am), (_amAgreementName am, agreementShowFn (ViewAgreementRevision (_amAgreementId am) (_amRevisionId am) )))
                            update (map mkPair ams)
         | otherwise -> debugStrLn $ "agreementsRequiredHandler - status = " ++ show status
       _ -> pure ()

-- * Query the server
remote :: (Show (RequestData url), Show (ResponseData url), SafeCopy (RequestData url), SafeCopy (ResponseData url), WithURL url) => StdMethod -> TaggedURL url AgreementsApiURL -> Maybe (RequestData url) -> (ResponseData url -> IO ()) -> IO ()
remote method (TaggedURL apiUrl) mReq callback =
  do debugStrLn "remote - start"
     xhr <- newXMLHttpRequest
--     let f = apiUrl
--     print f
     setResponseType xhr "arraybuffer"
     let settingsHandler ev =
           do debugStrLn "remote - readystatechange"
              rs <- getReadyState xhr
              debugStrLn $ "remote - readstate = " ++ show rs
              case rs of
                4 -> do
                  status <- getStatus xhr
                  ty     <- getResponseType xhr
                  mbs    <- getResponseByteString xhr
                  debugStrLn $ "remote - status = " ++ show status ++ " , type = " ++ Text.unpack ty ++ " , text = " ++ (maybe "Nothing" BS.unpack mbs) ++ " $"
                  case mbs of
                    Nothing -> pure ()
                    (Just bs) ->
                      case runGet safeGet bs of
                        (Left e) ->
                          do debugStrLn e
                             pure ()
                        (Right r) ->
                          do debugStrLn $ "remote - res - " ++ show r
                             callback r
                _ -> pure ()
     addEventListener xhr (ev @ReadyStateChange) settingsHandler False
--     cbu <- clckwrksBaseURL <$> (atomically $ readTVar modelTV)
     -- FIXME: this should not be hard coded
     let agreementsShowFn :: AgreementsURL -> Text
         agreementsShowFn = (\u -> "/" <> agreementsPluginName <> toPathInfo u)
     let url = agreementsShowFn (AgreementsApi RecordAgreed)
     -- print url
     open xhr (Text.decodeLatin1 (renderStdMethod method)) url True
     case mReq of
       Nothing    -> send xhr
       (Just req) ->
         do debugStrLn $ "remote - sending request value = " ++ show req
            setRequestHeader xhr "Content-Type" "application/octet-stream"
            sendArrayBuffer xhr (byteStringToArrayBuffer (runPut (safePut req)))


postAgreedRecord :: [(AgreementId, RevisionId)] -> IO ()
postAgreedRecord ars =
  do debugStrLn "postAgreedRecord"
     remote POST (withURL @RecordAgreed) (Just ars) $ \() ->
       do debugStrLn "AgreedRecord sent."

agreementsSignupPlugin' :: (AgreementsURL -> Text) -> SignupPlugin
agreementsSignupPlugin' agreementsRouteFn = SignupPlugin
  { spHTML     = do (Just d) <- currentDocument
                    (n, update) <- agreementsSignupForm d
--                    update []
                    -- fetch the required agreements
                    xhr <- newXMLHttpRequest
                    setResponseType xhr "arraybuffer"
                    open xhr "GET" (agreementsRouteFn AgreementsRequired) True
                    addEventListener xhr (ev @ReadyStateChange) (agreementsRequiredHandler agreementsRouteFn update xhr) False
                    send xhr
                    -- appendChild parent n
                    pure n
  , spValidate = \rootElem ->
      do me <- getElementByNameAttr rootElem "agreements-list"
         case me of
           Nothing ->
             do debugStrLn "agreementsSignupPlugin: could not find element with name=agreements-list"
                pure Nothing
           (Just e) ->
             do ms <- getData e "agreements"
                case ms of
                  Nothing ->
                    do debugStrLn "agreementsSignupPlugin: could not get data value 'agreements'"
                       pure Nothing
                  (Just s) ->
                    case readMaybe (JS.unpack s) of
                      Nothing ->
                        do debugStrLn "agreementsSignupPlugin: could not parse list of agreements"
                           pure Nothing
                      (Just ars) ->
                        pure $ Just $ map (\(a,r) -> (AgreementId a, RevisionId r)) ars
{-
         me <- getElementByNameAttr rootElem "dp-i-agree"
         case me of
           Nothing ->
             do debugStrLn "dummyPlugin: could not find element with name=dp-i-agree"
                pure Nothing
           (Just e) ->
             do b <- getChecked e
                if b
                  then pure $ Just (AgreementId 1, RevisionId 1)
                  else pure $ Nothing
-}
  , spHandle = \ars uid ->
      do -- FIXME: in theory the required agreements could have changed between when they were original shown and now.
--         me <- getElementByNameAttr rootElem "dp-i-agree"
         putStrLn $ "looks like " ++ show uid ++ " agreed to = " ++ show ars
         postAgreedRecord ars
         pure ()
  }

-- | FIXME: how can plugins best find their base url
agreementsSignupPlugin = (agreementsPluginName, agreementsSignupPlugin' (\u -> "/" <> agreementsPluginName <> toPathInfo u))
