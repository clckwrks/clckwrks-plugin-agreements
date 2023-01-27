{-# language Arrows #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
module Chili.FormArrow where

import Control.Arrow        (Arrow(arr,first, second, (&&&), (***)), ArrowChoice((+++)), (<<<), (>>>),  returnA)
import Control.Category     (Category(id,(.)))
import Control.Monad        (when)
import Chili.Types (Event(Submit, Change, ReadyStateChange), EventObject, InputEvent(Input), InputEventObject(..), IsJSNode, JSDocument, JSElement, JSNode, JSNodeList, byteStringToArrayBuffer, createJSElement, createJSTextNode, ev, getLength, item, unJSNode, fromJSNode, getFirstChild, getOuterHTML, getValue, newXMLHttpRequest, nodeType, nodeValue, open, send, sendString, getStatus, getReadyState, getResponseByteString, getResponseText, getResponseType, getValue, parentNode, preventDefault, replaceChild, remove, sendArrayBuffer, setAttribute, setRequestHeader, setResponseType, setTextContent, setValue, stopPropagation, toJSNode)
import Dominator.Types (JSDocument, JSElement, JSNode, MouseEvent(..), MouseEventObject(..), addEventListener, fromEventTarget, getAttribute, getElementById, toJSNode, appendChild, currentDocument, removeChildren, target)
import Data.JSString.Text (textToJSString, textFromJSString)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding ((.), id)


newtype Validator c d = Validator (c -> IO (ValidationStatus d))

instance Category Validator where
  (Validator f) . (Validator g) = Validator $ \a ->
     do r <- g a
        case r of
          (ValidationSuccess "" b) -> f b
          (ValidationFailure ve)   -> pure $ ValidationFailure ve
  id = Validator $ \a -> pure $ ValidationSuccess "" a


data FormArrow b c where
  FormId         :: FormArrow b b
  FormFun        :: (b -> c) -> FormArrow b c
  FormFirst      :: FormArrow b c -> FormArrow (b, d) (c, d)
  FormSecond     :: FormArrow b c -> FormArrow (d, b) (d, c)
  FormElem       :: Text -> [(Text, Text)] -> FormArrow b c -> FormArrow b c
  FormElemAttrs  :: Text -> [(Text, Text)] -> (b -> [(Text, Text)]) -> FormArrow b c -> FormArrow b c
  FormLabel      :: Maybe Text -> Text -> FormArrow b c -> FormArrow b c
  FormInput      :: InputType -> Bool -> FormArrow a Text
  FormTextArea   :: Bool -> FormArrow Text Text
  FormCat        :: FormArrow c d -> FormArrow b c   -> FormArrow b d
  FormSplit      :: FormArrow b c -> FormArrow b' c' -> FormArrow (b, b') (c, c')
  FormSpan       :: Maybe Text -> Text -> FormArrow Text ()
---  FormValidator  :: (c -> IO (ValidationStatus d)) -> FormArrow (Either (ValidationStatus d) b) (Maybe c) -> FormArrow b (Maybe d)
  FormValidator  :: Validator c d -> FormArrow (Either (ValidationStatus d) b) (Maybe c) -> FormArrow b (Maybe d)
  FormErrorRight :: FormArrow b c -> FormArrow (ValidationStatus s) () -> FormArrow (Either (ValidationStatus s) b) (Maybe c)
--  FormValidator :: (c -> IO (ValidationStatus d)) -> Form (Maybe ValidationError) () -> FormArrow c (Maybe d)
--   FormChoice    :: FormArrow b c -> FormArrow b' c' -> FormArrow (Either b b') (Either c c')
{-
instance ArrowChoice FormArrow where
  f +++ g = FormChoice f g
-}
instance Show (FormArrow b c) where
  show (FormId) = "FormId"
  show (FormFun f) = "FormFun <fn>"
  show (FormFirst f) = "FormFirst (" ++ show f ++ ")"
  show (FormSecond f) = "FormSecond (" ++ show f ++ ")"
  show (FormElem nm attrs f) = "FormElem " ++ show nm ++ " " ++ show attrs ++ " (" ++ show f ++ ")"
  show (FormElemAttrs nm attrs attrsFn f) = "FormElemAttrs " ++ show attrs ++ " " ++ show nm ++ " <attrsFn> (" ++ show f ++ ")"
  show (FormLabel mCls txt f) = "FormLabel " ++ show mCls ++ " " ++ show txt ++ " (" ++ show f ++ ")"
  show (FormInput iType req) = "FormInput " ++ show iType ++ " " ++ show req
  show (FormTextArea req) = "FormTextArea " ++ show req
  show (FormCat b a) = "FormCat (" ++ show b ++ ") (" ++ show a ++ ")"
  show (FormSplit b a) = "FormSplit (" ++ show b ++ ") (" ++ show a ++ ")"
  show (FormSpan iText mClass) = "FormSpan " ++ show iText ++ " " ++ show mClass
  show (FormValidator _ a) = "FormValidator <fn> " ++ show a
  show (FormErrorRight a r) = "FormErrorRight (" ++ show a ++ ") (" ++ show r ++ ")"
--   show (FormChoice f g)    = "FormChoice (" ++ show f ++ ") (" ++ show g ++ ")"

data InputType
  = InputText
  | InputSubmit
  deriving (Eq, Ord, Read, Show)

inputType :: InputType -> Text
inputType InputText   = "text"
inputType InputSubmit = "submit"

instance Category FormArrow where
  id = FormFun id
  a . b = FormCat a b

instance Arrow FormArrow where
  arr f  = FormFun f
--  first  = FormFirst
--  second = FormSecond
  a *** b = FormSplit a b
--  a &&& b = error "&&& not implemented"

renderForm :: JSNode -> JSDocument -> FormArrow b c -> IO (b -> IO c)
renderForm parent d frm =
  case frm of
    (FormFun f) ->
      pure $ \b -> pure (f b)

    (FormInput it required) ->
      do (Just e) <- createJSElement d "input"
         setAttribute e "type" (inputType it)
         when required $ setAttribute e "required" ""
--         setValue e init
         appendChild parent e
         pure $ \_ ->
           do (Just v) <- fmap textFromJSString <$> getValue e
              putStrLn $ "FormInput = " ++ show v
              pure v

    (FormCat f g) ->
      do gg <- renderForm parent d g
         fg <- renderForm parent d f
         pure $ (\b ->
                   do c <- gg b
                      fg c)

    (FormSplit f g) ->
      do fg <- renderForm parent d f
         gg <- renderForm parent d g
         pure $ \(b, b') ->
           do c  <- fg b
              c' <- gg b'
              pure (c, c')

    (FormErrorRight f er) ->
      do fg <- renderForm parent d f
         eg <- renderForm parent d er
         pure $ \x ->
           case x of
             (Left ve) ->
               do -- putStrLn $ "FormErrorRight - " ++ show ve
                  eg ve
                  pure Nothing
             (Right b) ->
               do c <- fg b
                  pure (Just c)


    (FormValidator (Validator validator) f) ->
      do fg <- renderForm parent d f
         pure $ \b ->
           do putStrLn $ "FormValidator"
              mc <- fg (Right b)
              case mc of
                Nothing ->
                  do putStrLn "FormValidator - mc = Nothing"
                     pure Nothing
                (Just c) ->
                  do putStrLn "FormValidator - validating"
                     ve <- validator c
                     case ve of
                       ValidationSuccess _ d ->
                         do putStrLn "FormValidator - validation success"
                            fg (Left ve)
                            pure $ Just d
                       ValidationFailure vf ->
                         do putStrLn "FormValidator - validation failure"
                            fg (Left ve)
                            pure $ Nothing

    FormSpan mClass iText ->
      do (Just e) <- createJSElement d "span"
         setTextContent e iText
         appendChild parent e
         case mClass of
           Nothing -> pure ()
           (Just cls) ->
             setAttribute e "class" cls
         pure $ \t ->
           do putStrLn "update span contents"
              setTextContent e t

    (FormElem nm attrs f) ->
      do (Just e) <- createJSElement d nm
         r <- renderForm (toJSNode e) d f
         mapM_ (\(a,v) -> setAttribute e a v) attrs
         appendChild parent e
         pure $ (\b -> do c <- r b
                          pure c
                )

    (FormElemAttrs nm initAttrs attrsFn f) ->
      do (Just e) <- createJSElement d nm
         r <- renderForm (toJSNode e) d f
         mapM_ (\(a,v) -> setAttribute e a v) initAttrs
         appendChild parent e
         pure $ (\b -> do c <- r b
                          mapM_ (\(a,v) -> setAttribute e a v) (attrsFn b)
                          pure c
                )
{-
         where
           update :: JSNode -> Text -> IO ()
           update spanNode newText =
             do putStrLn $ "update span with " ++ show newText
                setTextContent spanNode newText
-}
{-
           
           do mc <- fg b
              case mc of
                Nothing -> pure Nothing
                (Just c) ->
                  do vs <- validator c
                     case vs of
                       (ValidationSuccess _ d) -> pure $ Just d
                       _                       -> pure $ Nothing
-}
{-
                  (s1, g1) <- renderForm parent d a v
         (s2, g2) <- renderForm parent d er ValidationNone
-}
    _ -> error $ "renderForm: not implemented - " ++ show frm
{-
         pure $ (putter e, getter e)
           where
             putter e newValue = setValue e newValue
             getter e =
               do (Just v) <- fmap textFromJSString <$> getValue e
                  putStrLn $ "FormInput = " ++ show v
                  pure v -- (textFromJSString . fromJust) <$> getValue e
-}
{-
renderForm :: JSNode -> JSDocument -> FormArrow b c -> b -> IO (b -> IO (), IO c)
renderForm parent d frm init =
  case frm of
    FormId ->
      do ref <- newIORef init
         pure $ (\b -> writeIORef ref b, readIORef ref)

    (FormFun f) ->
      do ref <- newIORef (f init)
         pure $ (\b -> writeIORef ref (f b), readIORef ref)

    (FormFirst frm') ->
      do (s,g) <- renderForm parent d frm' (fst init)
         ref <- newIORef (snd init)

         pure (\(b, d) ->
                 do writeIORef ref d
                    s b
              , do c <- g
                   d <- readIORef ref
                   pure (c, d)
              )

    (FormSecond frm') ->
      do (s,g) <- renderForm parent d frm' (snd init)
         ref <- newIORef (fst init)

         pure (\(d, b) ->
                 do writeIORef ref d
                    s b
              , do c <- g
                   d <- readIORef ref
                   pure (d, c)
              )


    (FormInput it required) ->
      do (Just e) <- createJSElement d "input"
         setAttribute e "type" (inputType it)
         when required $ setAttribute e "required" ""
         setValue e init
         appendChild parent e
         pure $ (putter e, getter e)
           where
             putter e newValue = setValue e newValue
             getter e =
               do (Just v) <- fmap textFromJSString <$> getValue e
                  putStrLn $ "FormInput = " ++ show v
                  pure v -- (textFromJSString . fromJust) <$> getValue e

    (FormTextArea required) ->
      do (Just e) <- createJSElement d "textarea"
         when required $ setAttribute e "required" ""
         appendChild parent e
         pure $ (putter e, getter e)
           where
             putter e newValue = setValue e newValue
             getter e = (textFromJSString . fromJust) <$> getValue e

    (FormSplit c1 c2) ->
      do (s1,g1) <- renderForm parent d c1 (fst init)
         (s2,g2) <- renderForm parent d c2 (snd init)
         pure $ (\(b, b') ->
                   do s1 b
                      s2 b'
                , do c  <- g1
                     c' <- g2
                     pure ((c, c'))
                )
    (FormCat f g) ->
      do (sg,gg) <- renderForm parent d g init
         c <- gg
         (sf,gf) <- renderForm parent d f c
         pure $ (\b -> do sg b
                          c <- gg
                          sf c
                          pure ()
                , do c <- gg
                     sf c
                     gf
                )
         {-
         
--         (s1,g1) <- renderForm parent d f init
  --       c <- g1
         
         pure $ (\b -> do s1 b
                          c <- g1
                          s2 c
                , do g2
                )
-}
{-
    (FormCat f g) ->
      do (s1,g1) <- renderForm parent d f init
         c <- g1
         (s2,g2) <- renderForm parent d g c
         pure $ (\b -> do s1 b
                          c <- g1
                          s2 c
                , do g2
                )
-}
{-
         pure $ (\x ->
                   do y <- r2 x
                      r1 y)
-}
    FormSpan mClass ->
      do (Just e) <- createJSElement d "span"
         setTextContent e init
         appendChild parent e
         case mClass of
           Nothing -> pure ()
           (Just cls) ->
             setAttribute e "class" cls
         pure $ (update (toJSNode e), pure ())
         where
           update :: JSNode -> Text -> IO ()
           update spanNode newText =
             do putStrLn $ "update span with " ++ show newText
                setTextContent spanNode newText

    (FormElem nm attrs f) ->
      do (Just e) <- createJSElement d nm
         mapM_ (\(a,v) -> setAttribute e a v) attrs
         r <- renderForm (toJSNode e) d f init
         appendChild parent e
         pure r

    (FormElemAttrs nm attrsFn f) ->
      do (Just e) <- createJSElement d nm
         -- mapM_ (\(a,v) -> setAttribute e a v) attrs
         (s, g) <- renderForm (toJSNode e) d f init
         appendChild parent e
         pure (\x -> do s x
                        mapM_ (\(a,v) -> setAttribute e a v) (attrsFn x)
              , g
              )

    (FormErrorRight a er) ->
      do let (Right v) = init
         (s1, g1) <- renderForm parent d a v
         (s2, g2) <- renderForm parent d er ValidationNone
         pure (\x ->
                 case x of
                   (Left v) -> s2 v
                   (Right c) -> s1 c
              , do x <- g1
                   g2
                   pure x
              )

{-  This would be nice -- but the init value is Either c c' and we would need (c, c') to init this
    (FormChoice f g) ->
      do (s1, g1) <- renderForm parent d f undefined
         (s2, g2) <- renderForm parent d g undefined
         pure $ undefined
-}

    (FormValidator validator frm) ->
      do (s, g) <- renderForm parent d frm (Right init)
         let g' = do c <- g
                     v <- validator c
                     case v of
                       (ValidationSuccess _ d) ->
                         do s (Left ValidationNone)
                            pure $ Just d
                       (ValidationFailure er)  ->
                         do s (Left er)
                            pure Nothing
         pure (\x -> s (Right x), g')

    _ -> error $ "renderForm: not implemented - " ++ show frm
-}
{-
renderForm :: JSNode -> JSDocument -> FormArrow b c -> IO (b -> IO c)
renderForm parent d frm =
  case frm of
    (FormFun f) -> pure $ (\a -> pure $ f a)

    (FormSplit c1 c2) ->
      do r1 <- renderForm parent d c1
         r2 <- renderForm parent d c2
         pure $ (\(b, b') ->
                   do c <- r1 b
                      c' <- r2 b'
                      pure (c, c'))

    (FormElem nm attrs f) ->
      do (Just e) <- createJSElement d nm
         mapM_ (\(a,v) -> setAttribute e a v) attrs
         r <- renderForm (toJSNode e) d f
         appendChild parent e
         pure r

    FormSpan mClass ->
      do (Just e) <- createJSElement d "span"
         appendChild parent e
         case mClass of
           Nothing -> pure ()
           (Just cls) ->
             setAttribute e "class" cls
         pure $ update (toJSNode e)
         where
           update :: JSNode -> Text -> IO ()
           update spanNode newText =
             do putStrLn $ "update span with " ++ show newText
                setTextContent spanNode newText

    (FormInput it required) ->
      do (Just e) <- createJSElement d "input"
         setAttribute e "type" (inputType it)
         when required $ setAttribute e "required" ""
         appendChild parent e
         pure $ (\() -> pure ())

    (FormTextArea required) ->
      do (Just e) <- createJSElement d "textarea"
         when required $ setAttribute e "required" ""
         appendChild parent e
         pure $ (\() -> pure ())

    (FormLabel mClass labelTxt frm) ->
      do (Just lblE) <- createJSElement d "label"
         (Just txtNode) <- createJSTextNode d labelTxt
         case mClass of
           Nothing -> pure ()
           (Just cls) -> setAttribute lblE "class" cls
         appendChild lblE txtNode
         appendChild parent lblE

         renderForm parent d frm

    (FormCat b a) ->
      do r2 <- renderForm parent d a
         r1 <- renderForm parent d b
         pure $ (\x ->
                   do y <- r2 x
                      r1 y)
-}
{-

-}
{-
renderForm f =
  [domc|
      <form>
       <f-mk-ctrls></f-mk-ctrls>
      </form>
      |]
    where
      mkCtrls :: JSDocument -> IO (JSNode, FormArrow () () -> IO ())
      mkCtrls =
        [domc|
             <input type='text' required>
             |]
--    :: FormArrow b c -> JSDocument -> (JSNode, Model -> IO ())
-}

div_ :: Text -> FormArrow b c -> FormArrow b c
div_ cls frm = FormElem "div" [("class", cls)] frm

fieldset_ cls frm = FormElem "fieldset" [("class",cls)] frm
{-
newAgreementForm :: FormArrow Text Text
newAgreementForm =
  div_ "form-horizontal" $
   fieldset_ "reform" $
    FormCat (div_ "control-group" $ FormLabel (Just "control-label") "Agreement" $ div_ "controls" $ FormTextArea True)
            (FormCat (div_ "control-group" $
                       (FormLabel (Just "control-label") "Update Note" $ div_ "controls" $ FormInput InputText True))
                     (div_ "control-group" $ FormLabel (Just "control-label") "Agreement Name" $ div_ "controls" $ FormInput InputText True)
            )
-}


data ValidationStatus a
  = ValidationFailure ValidationError
  | ValidationSuccess Text a
  deriving Show

data ValidationError
  = ValidationWarning Text
  | ValidationError Text
  | ValidationInfo Text
  | ValidationNone
  deriving Show

validationErrorText :: ValidationStatus a -> Text
validationErrorText (ValidationFailure (ValidationWarning t)) = t
validationErrorText (ValidationFailure (ValidationError t))   = t
validationErrorText (ValidationFailure (ValidationInfo t))    = t
validationErrorText (ValidationFailure ValidationNone)        = ""
validationErrorText (ValidationSuccess t _)                   = t

errorSpan :: FormArrow (ValidationStatus a) ()
errorSpan = FormFun validationErrorText >>> FormSpan (Just "help-inline") ""

controlGroup :: FormArrow (Either (ValidationStatus s) b) c -> FormArrow (Either (ValidationStatus s) b) c
controlGroup frm = divCG frm
  where
   divCG :: FormArrow (Either (ValidationStatus s) b) c -> FormArrow (Either (ValidationStatus s) b) c
   divCG frm = FormElemAttrs "div" [("class","control-group")] mkAttrs frm
     where
       mkAttrs (Right _)                                         = [("class","control-group")]
       mkAttrs (Left (ValidationSuccess _ _))                    = [("class","control-group success")]
       mkAttrs (Left (ValidationFailure (ValidationWarning {}))) = [("class","control-group warning")]
       mkAttrs (Left (ValidationFailure (ValidationError {})))   = [("class","control-group error")]
       mkAttrs (Left (ValidationFailure (ValidationInfo {})))    = [("class","control-group info")]
       mkAttrs (Left (ValidationFailure (ValidationNone {})))    = [("class","control-group")]

{-
controlGroup :: (c -> IO (ValidationStatus d)) -> FormArrow b c -> FormArrow b (Maybe d)
controlGroup validator frm =
  proc b ->
    do -- c <- divCG $ FormValidator validator undefined {- (frm +++ errorSpan) -} -< b
       c <- FormValidator validator $ divCG $
         div_ "controls" $
           FormErrorRight frm errorSpan  -< b
       returnA -< c
  where
   divCG :: FormArrow (Either ValidationError b) c -> FormArrow (Either ValidationError b) c
   divCG frm = FormElemAttrs "div" mkAttrs frm
     where
       mkAttrs (Right _)                     = [("class","control-group")]
       mkAttrs (Left (ValidationWarning {})) = [("class","control-group warning")]
       mkAttrs (Left (ValidationError {}))   = [("class","control-group error")]
       mkAttrs (Left (ValidationInfo {}))    = [("class","control-group info")]
       mkAttrs (Left (ValidationNone {}))    = [("class","control-group")]
-}
{-
  where
    cgClass ValidationNone = "control-group"
-}


-- newAgreementForm :: FormArrow (Text, Text) (Maybe Text)
{-
newAgreementForm :: FormArrow (Text, Text) (Maybe Text)
newAgreementForm =
  proc txt ->
   do newText <- FormValidator equalText $ fanError >>> FormSplit
        (controlGroup $ div_ "controls" (FormErrorRight (FormInput InputText True) errorSpan))
        (controlGroup $ div_ "controls" (FormErrorRight (FormInput InputText True) errorSpan)) -< txt
      _ <- div_ "control-group" $ div_ "controls" (FormInput InputSubmit True) -< "Submit"
      returnA -< newText
-}
{-
newAgreementForm :: FormArrow (Text, Text) (Text, Text)
newAgreementForm =
   proc txt ->
     do r <- (FormSplit (FormInput InputText True) (FormInput InputText True)) -< txt
        returnA -< r

{-
        (controlGroup $ div_ "controls" (FormErrorRight (FormInput InputText True) errorSpan))
        (controlGroup $ div_ "controls" (FormErrorRight (FormInput InputText True) errorSpan)) -< txt
      _ <- div_ "control-group" $ div_ "controls" (FormInput InputSubmit True) -< "Submit"
      returnA -< newText
-}
  where
--     fanError :: FormArrow (Either ValidationError Text, Either ValidationError Text) (Text, Text) -> FormArrow (Either ValidationError (Text, Text)) (Text, Text)
    fanError = (FormFun $ \x ->
      case x of
        (Left err) -> (Left err, Left err)
        (Right (x,y)) -> (Right x, Right y))

    nonEmptyText :: Text -> IO (ValidationStatus Text)
    nonEmptyText t
      | Text.null t = pure $ ValidationFailure $ ValidationError "field can not be empty"
      | otherwise   = pure $ ValidationSuccess "" t


    equalText :: (Text, Text) -> IO (ValidationStatus Text)
    equalText (c, c')
      | c == c'   = pure $ ValidationSuccess "" c
      | otherwise = pure $ ValidationFailure $ ValidationError "fields are not the same"
-}
{-
composedForm :: FormArrow Text Text
composedForm =
  FormCat (FormInput InputText False) (FormInput InputText False)
-}

nonEmptyText :: Text -> IO (ValidationStatus Text)
nonEmptyText t
      | Text.null t = pure $ ValidationFailure (ValidationError "field can not be empty")
      | otherwise   = pure $ ValidationSuccess "" t

nonEmptyTextV = Validator nonEmptyText
fanError = (FormFun $ \x ->
      case x of
        (Left err) -> (Left err, Left err)
        (Right (x,y)) -> (Right x, Right y))

equalText :: Text -> (Text, Text) -> IO (ValidationStatus Text)
equalText errMsg (c, c')
  | c == c'   = pure $ ValidationSuccess "" c
  | otherwise = pure $ ValidationFailure $ ValidationError errMsg

equalTextV errMsg = Validator (equalText errMsg)


maybeMaybe :: FormArrow (Maybe Text, Maybe Text) (Maybe (Text, Text))
maybeMaybe = FormFun $
  \(ma, mb) ->
    case (ma, mb) of
      (Just a, Just b) -> Just (a, b)
      _                -> Nothing

label :: Text -> FormArrow a a
label txt =
  proc a ->
    do FormElem "label" [("class", "control-label")] (FormSpan Nothing txt) -< txt
       returnA -< a
