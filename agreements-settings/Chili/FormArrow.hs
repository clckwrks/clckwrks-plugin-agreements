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
  FormInput      :: InputType -> Bool -> FormArrow Text Text
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

instance Functor (FormArrow b) where
  fmap f FormId = FormFun f
  fmap f (FormFun g) = FormFun $ f . g
  fmap f frm = FormCat (FormFun f) frm

{- This should be possible, but not worried about it yet.

instance Applicative (FormArrow b) where
  pure c = FormFun (\_ -> c)
--  f <*> x = arr (uncurry (flip ($))) . first x . arr swap . first f . arr dup
  (FormFun f) <*> frm =
    FormFun $ \a -> fmap (f a) frm
-}

{-
   where
     dup x = (x,x)
     swap (x,y) = (x,y)
-}
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

data FormAction
  = SetValue
  | Validate
  deriving Show

renderForm :: JSNode -> JSDocument -> FormArrow b c -> IO ((FormAction, b) -> IO c)
renderForm parent d frm =
  case frm of
    (FormFun f) ->
      pure $ \(_, b) -> pure (f b)

    (FormInput iType required) ->
      do (Just e) <- createJSElement d "input"
         setAttribute e "type" (inputType iType)
         when required $ setAttribute e "required" ""
--         setValue e init
         appendChild parent e
         pure $ \(action, val) ->
           case action of
             Validate ->
               do (Just v) <- fmap textFromJSString <$> getValue e
                  putStrLn $ "FormInput = " ++ show v
                  pure v
             SetValue ->
               do setValue e val
                  pure val

    (FormCat f g) ->
      do gg <- renderForm parent d g
         fg <- renderForm parent d f
         pure $ (\b@(action,_) ->
                    do c <- gg b
                       fg (action, c)
                )


    (FormSplit f g) ->
      do fg <- renderForm parent d f
         gg <- renderForm parent d g
         pure $ \(action, (b, b')) ->
           do c  <- fg (action, b)
              c' <- gg (action, b')
              pure (c, c')

    (FormErrorRight f er) ->
      do fg <- renderForm parent d f
         eg <- renderForm parent d er
         pure $ \(action, x) ->
           case x of
             (Left ve) ->
               do -- putStrLn $ "FormErrorRight - " ++ show ve
                  eg (action, ve)
                  pure Nothing
             (Right b) ->
               do c <- fg (action, b)
                  pure (Just c)

    (FormValidator (Validator validator) f) ->
      do fg <- renderForm parent d f
         pure $ \(action, b) ->
           do putStrLn $ "FormValidator"
              mc <- fg (action, Right b)
              case action of
                SetValue -> pure Nothing
                Validate ->
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
                                fg (action, Left ve)
                                pure $ Just d
                           ValidationFailure vf ->
                             do putStrLn "FormValidator - validation failure"
                                fg (action, Left ve)
                                pure $ Nothing

    FormSpan mClass iText ->
      do (Just e) <- createJSElement d "span"
         setTextContent e iText
         appendChild parent e
         case mClass of
           Nothing -> pure ()
           (Just cls) ->
             setAttribute e "class" cls
         pure $ \(action,t) ->
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
         pure $ (\(action,b) ->
                       do c <- r (action, b)
                          mapM_ (\(a,v) -> setAttribute e a v) (attrsFn b)
                          pure c
                )

    _ -> error $ "renderForm: not implemented - " ++ show frm

div_ :: Text -> FormArrow b c -> FormArrow b c
div_ cls frm = FormElem "div" [("class", cls)] frm

fieldset_ cls frm = FormElem "fieldset" [("class",cls)] frm

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

-- eitherMerge :: (Either (ValidationStatus s) (Maybe Text), Either (ValidationStatus s) (Maybe Text)) -> Either (ValidationStatus sa) (Maybe Text, Maybe Text)
-- eitherMerge = undefined

eitherSplit :: FormArrow (Either (ValidationStatus s) (c, d)) (Either (ValidationStatus s) c, Either (ValidationStatus s) d)
eitherSplit = FormFun $
  \e ->
    case e of
      (Left vs)       -> (Left vs, Left vs)
      (Right (t1,t2)) -> (Right t1, Right t2)

label :: Text -> FormArrow a a
label txt =
  proc a ->
    do FormElem "label" [("class", "control-label")] (FormSpan Nothing txt) -< txt
       returnA -< a
