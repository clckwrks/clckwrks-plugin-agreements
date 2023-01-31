{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# language QuasiQuotes, TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow        (Arrow(arr,first, second, (&&&), (***)), ArrowChoice((+++)), (<<<), (>>>),  returnA)
import Control.Category     (Category(id,(.)))
import Control.Monad        ((<=<), when)
import Clckwrks.Agreements.Types (AgreementId(..), AgreementMeta(..), AgreementsSettings(..), RevisionId(..))
import Clckwrks.Agreements.URL (AgreementsURL(..), AgreementsAdminURL(..), AgreementsAdminApiURL(..))
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar', readTVar, writeTVar)
import Control.Concurrent.STM (atomically)
import Chili.FormArrow
import Chili.Types (Event(Submit, Change, ReadyStateChange), EventObject, InputEvent(Input), InputEventObject(..), IsJSNode, JSElement, JSNode, JSNodeList, byteStringToArrayBuffer, createJSElement, createJSTextNode, ev, getLength, item, unJSNode, fromJSNode, getFirstChild, getOuterHTML, getValue, newXMLHttpRequest, nodeType, nodeValue, open, send, sendString, getStatus, getReadyState, getResponseByteString, getResponseText, getResponseType, getValue, parentNode, preventDefault, replaceChild, remove, sendArrayBuffer, setAttribute, setRequestHeader, setResponseType, setTextContent, setValue, stopPropagation, createJSTextNode, createJSElement)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toUpper)
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import Data.Maybe (fromMaybe, fromJust)
import Data.SafeCopy (SafeCopy, safeGet, safePut)
import Data.Serialize (runGet, runPut)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.UserId(UserId(..))
import Dominator.Types (JSDocument, JSElement, JSNode, MouseEvent(..), MouseEventObject(..), addEventListener, fromEventTarget, getAttribute, getElementById, toJSNode, appendChild, currentDocument, removeChildren, target)
import Dominator.DOMC
import Dominator.JSDOM
import Data.IORef        (IORef, newIORef, writeIORef, readIORef)
import Network.HTTP.Types (StdMethod(GET, POST), renderStdMethod)
import Language.Haskell.TH (Name, ExpQ, mkName)
import Language.Haskell.TH.Syntax (qAddDependentFile)
import Prelude       hiding ((.), id)
import System.IO (hFlush, stdout, hGetBuffering, hSetBuffering, BufferMode(..))
import Text.Read (readMaybe)
import Web.Routes

debugStrLn :: String -> IO ()
debugStr :: String -> IO ()
debugPrint :: (Show a) => a -> IO ()
#define DEBUG_CLIENT
#ifdef DEBUG_CLIENT
debugStrLn = putStrLn
debugStr   = putStr
debugPrint = print
#else
debugStrLn = const $ pure ()
debugStr   = const $ pure ()
debugPrint = const $ pure ()
#endif

type Lang = Text

showNode :: (IsJSNode n, MonadIO m) => n -> m String
showNode n' = liftIO $
  do let n = toJSNode n'
     nt <- nodeType n
     case nt of
       1 -> do let (Just e) = fromJSNode @JSElement n
               h <- getOuterHTML e
               pure (JS.unpack h)
       3 -> do v <- nodeValue n
               pure ("TextNode -> '" ++ JS.unpack v ++ "'")
       _ -> pure $ "nodeType -> " ++ show nt

printNode :: (IsJSNode n) => n -> IO ()
printNode n = debugStrLn =<< showNode n

printNodeList :: JSNodeList -> IO ()
printNodeList nl =
  do l <- getLength nl
     mapM_ (\c -> do (Just n) <- item nl c ; printNode n) [0..(l - 1)]

data Model = Model
  { clckwrksBaseURL    :: Text
  , agreementsBaseURL  :: Maybe Text
  , agreementsMeta     :: [AgreementMeta]
  }
  deriving Show

initModel :: UTCTime -> Model
initModel now = Model
  { clckwrksBaseURL   = "/agreements"
  , agreementsBaseURL  = Nothing
  , agreementsMeta     = [ AgreementMeta { _amAgreementId    = AgreementId 1
                                         , _amAgreementName  = "sample agreement"
                                         , _amRevisionId     = RevisionId 1
                                         , _amRevisionDate   = now
                                         , _amRevisionNote   = "This is just a test"
                                         , _amRevisionAuthor = UserId 1
                                         }

                         ]
  }

data FieldName
  = UpdateAgreement
  deriving (Eq, Ord, Read, Show)

remote :: (Show req, Show res, SafeCopy req, SafeCopy res) => TVar Model -> StdMethod -> AgreementsAdminApiURL -> Maybe req -> (res -> IO ()) -> IO ()
remote modelTV method apiUrl mReq callback =
  do xhr <- newXMLHttpRequest
     setResponseType xhr "arraybuffer"
     let settingsHandler ev =
           do putStrLn "settingsHandler - readystatechange"
              rs <- getReadyState xhr
              putStrLn $ "settingsHandler - readstate = " ++ show rs
              case rs of
                4 -> do
                  status <- getStatus xhr
                  ty     <- getResponseType xhr
                  mbs    <- getResponseByteString xhr
                  putStrLn $ "status = " ++ show status ++ " , type = " ++ Text.unpack ty ++ " , text = " ++ (maybe "Nothing" BS.unpack mbs) ++ " $"
                  case mbs of
                    Nothing -> pure ()
                    (Just bs) ->
                      case runGet safeGet bs of
                        (Left e) ->
                          do putStrLn e
                             pure ()
                        (Right r) ->
                          do putStrLn $ "res - " ++ show r
                             callback r
                _ -> pure ()
     addEventListener xhr (ev @ReadyStateChange) settingsHandler False
     cbu <- clckwrksBaseURL <$> (atomically $ readTVar modelTV)
     let url = cbu  <> toPathInfo (AgreementsAdmin (AgreementsAdminApi apiUrl))
     print url
     open xhr (Text.decodeLatin1 (renderStdMethod method)) url True
     case mReq of
       Nothing    -> send xhr
       (Just req) ->
         do putStrLn $ "sending request value = " ++ show req
            setRequestHeader xhr "Content-Type" "application/octet-stream"
            sendArrayBuffer xhr (byteStringToArrayBuffer (runPut (safePut req)))

noReq :: Maybe ()
noReq = Nothing

{-
getListsRemote update modelTV =
  remote modelTV GET GetLists noReq (\l -> do
                                        putStrLn $ "GetLists returned - " ++ show (l :: [(ListId, Text)])
                                        atomically $ modifyTVar' modelTV $ \m -> m { lists = l }
                                        update =<< (atomically $ readTVar modelTV)
                                    )
-}
main :: IO ()
main =
  do hSetBuffering stdout LineBuffering
     (Just d) <- currentDocument
---     (newNode, update) <- template d
     (newNode, update, getter) <- newAgreementTemplate d
     me <- getElementById d "pagelet-div"
     case me of
       Nothing -> pure ()
       (Just rootNode) ->
         do mp <- parentNode rootNode
            case mp of
              Nothing -> pure ()
              (Just p) ->
                do -- attach pagelet
                   replaceChild p newNode rootNode

                   -- init the model and update view
                   -- we do the update here so that we do not see the mustache syntax while waiting for getList to return
                   now <- getCurrentTime
                   modelTV <- newTVarIO (initModel now)
                   update =<< (atomically $ readTVar modelTV)

                   -- add event handlers
--                   addEventListener newNode (ev @Change) (changeHandler update modelTV) False
{-
                   addEventListener newNode (ev @Change) (\e ->
                      do preventDefault e
                         stopPropagation e
                         putStrLn "Change handler"
                         mVal <- getter ()
                         putStrLn $ "mVal = " ++ show mVal) False
-}
{-
                   addEventListener newNode (ev @Submit) (\e ->
                      do preventDefault e
                         stopPropagation e
                         putStrLn "Submit handler"
                         mVal <- getter (Nothing, Nothing)
                         putStrLn $ "mVal = " ++ show mVal) False
-}

                   -- get the AgreementsSettings
                   {-
                   remote modelTV GET GetAgreementsSettings noReq (\mcs ->
                                                                     do print (mcs :: AgreementsSettings)
                                                                        atomically $ modifyTVar' modelTV $ \m ->
                                                                          m { agreementsBaseURL  = _mbBaseURL mcs
                                                                            }
                                                                        update =<< (atomically $ readTVar modelTV)
                                                                 )
                    -}
                   putStrLn "init done."
                   pure ()

changeHandler :: (Model -> IO ()) -> TVar Model -> EventObject Change -> IO ()
changeHandler update modelTV e =
  do putStrLn "changeHandler"
     case fromEventTarget @JSElement (target e) of
       Nothing  -> do
         putStrLn "change not attached to an element"
         pure ()
       (Just elem) -> do
         mName <- getAttribute elem "name"
         mValue <- getValue elem
         debugStrLn $ "mName = " ++ show mName ++ " , mValue = " ++ show mValue

         case (readMaybe . JS.unpack) =<< mName of
           (Just UpdateAgreement) -> pure ()
{-
             do debugStrLn $ "BaseUrl value = " ++ show mValue
                let mBaseURL =
                      case mValue of
                        Just v | not (JS.null v) -> Just (textFromJSString v)
                        _ -> Nothing
                atomically $ modifyTVar' modelTV $ \m -> m { agreementsBaseURL = mBaseURL }
                remote modelTV POST SetAgreements agreements  (\() -> putStrLn "sent ok")
-}
           _ -> debugStrLn $  "could not find or parse name. mName = " ++ show mName
{-
template :: JSDocument -> IO (JSNode, Model -> IO ())
template = [domc|
  <div id="agreements-settings">
      <table>
       <thead>
        <tr>
         <th>Id</th>
         <th>Revision</th>
         <th>Name</th>
         <th>Date</th>
         <th>Author</th>
         <th>Note</th>
        </tr>
       </thead>
       <tbody>
        <f-agreement-list d-map="agreementsMeta model"></f-agreement-list>
       </tbody>
      </table>
  </div>
  |]
  where agreementList :: JSDocument -> IO (JSNode, AgreementMeta -> IO ())
        agreementList =
          [domc|
               <tr>
                 <td>{{ show $ _unAgreementId $ _amAgreementId model }}</td>
                 <td>{{ show $ _unRevisionId $ _amRevisionId model }}</td>
                 <td>{{ Text.unpack $ _amAgreementName model }}</td>
                 <td>{{ show $ _amRevisionDate model }}</td>
                 <td>{{ show $ _unUserId $ _amRevisionAuthor model }}</td>
                 <td>{{ Text.unpack $ _amRevisionNote model }}</td>
               </tr>
               |]
-}
--       <label for="agreements-base-url">Agreements Base URL</label><input id="agreements-base-url" name="{{show UpdateAgreement}}" type="text" placeholder="Base URL" value='{{ maybe "" Text.unpack (agreementsBaseURL model) }}'>
{-

It would be nice to have something like reform which packages forms up into nice validated data types.

Reform needs to do the validate after you press submit. But with client side forms, validation can happen in realtime.

In reform, all the form fields must appear at once. With a clientside library, it could be possible that a radio button or dropdown would change what fields are show in the form.

The 'Reform' type has to be an 'Applicative' because if one field failed to validate, it might not be possible to validate the remaining fields.

The problem of having the form change shape or potential values of one field depend on the current value of another field is tricky and may require arrows.

The end goal is to get back a data value that can only exist if all the data in it is valid. But while the form is being created, we may not be able to construct values of that type.


We have two types of validation: local and remote.

local validation does not require going to the server, but remote validation does. For remote validation we might be able to send the values bundled up in a type. But for local values -- we would only have a partial record.

When validating a single field at a time it is easy to know which field an error as attached to. But when validating multiple fields -- how do we may errors back to fields?

Do we really need to distinguish between local and remote? If we allow IO in the validation -- then any validator can be local or remote.



-}


simpleForm1 :: FormArrow (Text, Text) (Text, Text)
simpleForm1 =
  proc (t1,t2) ->
    do a <- FormInput InputText False -< t1
       b <- FormInput InputText False -< t2
       returnA -< (a, b)
{-
-- simpleForm :: FormArrow () Text
simpleForm2 = FormValidator nonEmptyTextV $ FormErrorRight (FormInput InputText False) errorSpan
-}
simpleForm3 :: FormArrow (Text, Text) (Maybe Text, Maybe Text)
simpleForm3 =
  proc (txt1,txt2) ->
    do x <- FormValidatorOnChange nonEmptyTextV "" $ controlGroup $ FormErrorRight (FormInput InputText False) errorSpan -< txt1
       y <- FormValidatorOnChange nonEmptyTextV "" $ controlGroup $ FormErrorRight (FormInput InputText False) errorSpan -< txt2
       FormInput InputSubmit False -< "Submit It"
       returnA -< (x,y)
{-
simpleForm4 =
  div_ "form-horizontal" $
             fieldset_ "reform" $
  proc (txt1,txt2) ->
    do x <- FormValidator (nonEmptyTextV . (equalTextV "password must match")) $ maybeMaybe <<<
               (((controlGroup $ label "one" >>> (div_ "controls" $ FormErrorRight (FormInput InputText False) errorSpan))) &&&
               (controlGroup $ label "two" >>> (div_ "controls" $ FormErrorRight (FormInput InputText False) errorSpan))) -< (Just "foo")
       div_ "control-group" $ (div_ "controls" $ FormInput InputSubmit False) -< Nothing
       returnA -< x

simpleForm5 =
  proc (txt2) ->
    do r <- (FormValidator nonEmptyTextV $ controlGroup $ label "one" >>> FormErrorRight (FormInput InputText False) errorSpan) &&&
            (FormValidator nonEmptyTextV $ controlGroup $ label "two" >>> FormErrorRight (FormInput InputText False) errorSpan) -< txt2
       FormInput InputSubmit False -< Nothing
       returnA -< r

simpleForm6 =
  proc (txt1, txt2) ->
    do r <- (FormValidator nonEmptyTextV $ controlGroup $ label "one" >>> FormErrorRight (FormInput InputText False) errorSpan) ***
            (FormValidator nonEmptyTextV $ controlGroup $ label "two" >>> FormErrorRight (FormInput InputText False) errorSpan) -< (txt1, txt2)
       div_ "controls" $ FormInput InputSubmit False -< Nothing
       returnA -< r
-}
simpleForm7 =
  div_ "form-horizontal" $
   fieldset_ "reform" $
    proc (txt1, txt2) ->
      do r <- FormValidatorOnChange (nonEmptyTextV <<< equalTextV "passwords must match") ("","") $ merged -< (txt1, txt2)
         div_ "controls" $ FormInput InputSubmit False -< "Submit"
         returnA -< r
           where
             one :: FormArrow (Either (ValidationStatus s0) (Text)) (Maybe Text)
             one = controlGroup $ label "one" >>> (div_ "controls" $ FormErrorRight (FormInput InputText False) errorSpan)

             two :: FormArrow (Either (ValidationStatus s0) (Text)) (Maybe Text)
             two = controlGroup $ label "two" >>> (div_ "controls" $ FormErrorRight (FormInput InputText False) errorSpan)

             combined :: FormArrow (Either (ValidationStatus s) ( Text), Either (ValidationStatus s) ( Text)) (Maybe Text, Maybe Text)
             combined = one *** two

--             merged' :: FormArrow (Either (ValidationStatus s) (Maybe Text), Either (ValidationStatus s) ( Text))  (Maybe (Text, Text))
--             merged' = combined >>> maybeMaybe

             merged = eitherSplit >>> combined >>> maybeMaybe

simpleForm8 =
  div_ "form-horizontal" $
   fieldset_ "reform" $
    proc (txt1, txt2) ->
      do r <- FormValidatorOnChange (equalTextV "passwords must match") ("","") $ merged  -< (txt1, txt2)
         div_ "controls" $ FormInput InputSubmit False -< "Submit"
         returnA -< r
           where
--              one :: FormArrow (Either (ValidationStatus s0) (Text)) (Maybe Text)
--             one :: FormArrow Text (Maybe Text)
             one = FormValidatorOnChange' nonEmptyTextV "" (controlGroup $ label "one" >>> (div_ "controls" $ FormErrorRight (FormInput InputText False) errorSpan))

--              two :: FormArrow (Either (ValidationStatus s0) (Text)) (Maybe Text)
--             two :: FormArrow Text (Maybe Text)
             two = FormValidatorOnChange' nonEmptyTextV "" (controlGroup $ label "two" >>> (div_ "controls" $ FormErrorRight (FormInput InputText False) errorSpan))

--             combined :: FormArrow (Either (ValidationStatus s) ( Text), Either (ValidationStatus s) ( Text)) (Maybe Text, Maybe Text)
             combined = one *** two

--             merged' :: FormArrow (Either (ValidationStatus s) (Maybe Text), Either (ValidationStatus s) ( Text))  (Maybe (Text, Text))
--             merged' = combined >>> maybeMaybe

             merged = eitherSplit >>> combined >>> maybeMaybe


simpleForm9 :: FormArrow (Text, Text, ListAction Text) (Maybe Text, [Text])
simpleForm9 =
  div_ "form-horizontal" $
   fieldset_ "reform" $
    proc (txt1, txt2, items) ->
      do r <- FormValidatorOnChange (equalTextV "passwords must match") ("","") $ merged  -< (txt1, txt2)
         rs <- FormList "div" (div_ "control" $ FormInput InputText False <<< arr (\mt -> (fromMaybe "" mt))) -< items
         _  <- FormOnClick "foo" (FormInput InputSubmit False) >>> FormInput InputText False -< "press me"
         div_ "controls" $ FormInput InputSubmit False -< "Submit"
         returnA -< (r, rs)
           where
--              one :: FormArrow (Either (ValidationStatus s0) (Text)) (Maybe Text)
--             one :: FormArrow Text (Maybe Text)
             one = FormValidatorOnChange' nonEmptyTextV "" (controlGroup $ label "one" >>> (div_ "controls" $ FormErrorRight (FormInput InputText False) errorSpan))

--              two :: FormArrow (Either (ValidationStatus s0) (Text)) (Maybe Text)
--             two :: FormArrow Text (Maybe Text)
             two = FormValidatorOnChange' nonEmptyTextV "" (controlGroup $ label "two" >>> (div_ "controls" $ FormErrorRight (FormInput InputText False) errorSpan))

--             combined :: FormArrow (Either (ValidationStatus s) ( Text), Either (ValidationStatus s) ( Text)) (Maybe Text, Maybe Text)
             combined = one *** two

--             merged' :: FormArrow (Either (ValidationStatus s) (Maybe Text), Either (ValidationStatus s) ( Text))  (Maybe (Text, Text))
--             merged' = combined >>> maybeMaybe

             merged = eitherSplit >>> combined >>> maybeMaybe

frm ++> err =
  FormErrorRight frm err

newAgreementForm :: FormArrow () (Maybe Text, Maybe Text, Maybe Text)
newAgreementForm =
  div_ "form-horizontal" $
   fieldset_ "reform" $
    (,,) <$> agreementName <*> agreementNote <*> agreementBody <* submitButton
  where
    agreementName :: FormArrow () (Maybe Text)
    agreementName =
      pure "" >>>
      FormValidatorOnChange nonEmptyTextV ""
        (controlGroup $ label "Agreement Name" >>>
          (div_ "controls" $ (FormInput InputText False) ++> errorSpan))

    agreementNote =
      pure "" >>>
      FormValidatorOnChange nonEmptyTextV ""
        (controlGroup $ label "Update Note" >>>
          (div_ "controls" $ (FormInput InputText False) ++> errorSpan))

    agreementBody =
      pure "" >>>
      FormValidatorOnChange nonEmptyTextV ""
        (controlGroup $ label "Agreement Contents (en-US)" >>>
          (div_ "controls" $ (FormTextArea False 2) ++> errorSpan))

    submitButton =
      div_ "controls" $ FormInput InputSubmit False <<< pure "Add Agreement"

{-
    FormCat (div_ "control-group" $ FormLabel (Just "control-label") "Agreement" $ div_ "controls" $ FormTextArea True)
            (FormCat (div_ "control-group" $
                       (FormLabel (Just "control-label") "Update Note" $ div_ "controls" $ FormInput InputText True))
                     (div_ "control-group" $ FormLabel (Just "control-label") "Agreement Name" $ div_ "controls" $ FormInput InputText True)
            )

-}
-- instance Category (


{-
  proc () ->
    do a <- FormInput InputText False -< ()
       b <- FormInput InputText False -< ()
       returnA -< (a, b)
-}
{-
newAgreementForm :: FormArrow Text ()
newAgreementForm =
  div_ "form-horizontal" $
   fieldset_ "reform" $
    proc t ->
      do an <- controlGroup $ FormLabel (Just "control-label") "Agreement Name" $
                 div_ "controls" $
                   (proc s ->
                      do FormInput InputText True -< ()
                         FormSpan (Just "help-inline") -< s) -< t
         _  <- div_ "control-group" $ (FormLabel (Just "control-label") "Update Note" $ div_ "controls" $ FormInput InputText True) -< ()
         _  <- div_ "control-group" $ (FormLabel (Just "control-label") "Agreement (en-US)" $ div_ "controls" $ FormTextArea True) -< ()
         _  <- div_ "control-group" $ div_ "controls" $ FormInput InputSubmit False -< ()
         returnA -< ()
{-
    FormCat (div_ "control-group" $ FormLabel (Just "control-label") "Agreement" $ div_ "controls" $ FormTextArea True)
            (FormCat (div_ "control-group" $

                     (div_ "control-group" $ FormLabel (Just "control-label") "Agreement Name" $ div_ "controls" $ FormInput InputText True)
            )
-}
-}
-- newAgreementTemplate :: JSDocument -> IO (JSNode, Model -> IO (), IO Text)
newAgreementTemplate d = mkCtls d newAgreementForm
  {- [domc|
  <div id="agreements-settings">
   <form>
     <f-mk-ctls></f-mk-ctls>
    </form>
  </div>
 |] -}
  where
--     mkCtls :: JSDocument -> IO (JSNode, Model -> IO (), (Text, Text) -> IO (Text, Text))
    mkCtls d frm =
      do (Just formN) <- fmap toJSNode <$> createJSElement d "form"
         print $ frm
         (nodes, getter) <- renderForm d frm -- ("Init Text 1", "Init Text 2")
         mapM_ (appendChild formN) nodes
         getter (SetValue, ())
--         getter (SetValue, ("", "", ListAppend ["4", "5", "6"]))

         (Just div) <- createJSElement d "div"
         (Just tn)  <- createJSTextNode d ""
         appendChild div tn
         appendChild formN div

         addEventListener formN (ev @Submit) (\e ->
                      do preventDefault e
                         stopPropagation e
                         putStrLn "Submit handler"
                         mVal <- getter (Validate, ())
                         putStrLn $ "mVal = " ++ show mVal
                         setTextContent div $ Text.pack $ show mVal
                                             ) False

         pure (formN, \_ -> pure (), getter)
