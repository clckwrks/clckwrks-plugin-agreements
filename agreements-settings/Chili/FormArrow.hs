{-# language Arrows #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module Chili.FormArrow where

import Control.Arrow        (Arrow(arr,first, second, (&&&), (***)), ArrowChoice((+++)), (<<<), (>>>),  returnA)
import Control.Category     (Category(id,(.)))
import Control.Monad        (replicateM, when, zipWithM)
import Chili.Types (Event(Submit, Change, ReadyStateChange), FocusEvent(FocusOut), EventObject, InputEvent(Input), InputEventObject(..), IsJSNode, JSDocument, JSElement, JSNode, JSNodeList, byteStringToArrayBuffer, createJSElement, createJSTextNode, ev, getLength, item, unJSNode, fromJSNode, getFirstChild, getOuterHTML, getValue, newXMLHttpRequest, nodeType, nodeValue, open, send, sendString, getStatus, getReadyState, getResponseByteString, getResponseText, getResponseType, getValue, parentNode, preventDefault, replaceChild, remove, sendArrayBuffer, setAttribute, setRequestHeader, setResponseType, setTextContent, setValue, stopPropagation, toJSNode)
import Dominator.Types (JSDocument, JSElement, JSNode, MouseEvent(..), MouseEventObject(..), addEventListener, fromEventTarget, getAttribute, getElementById, toJSNode, appendChild, currentDocument, removeChildren, target)
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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

data InputType
  = InputText
  | InputSubmit
  deriving (Eq, Ord, Read, Show)

inputType :: InputType -> Text
inputType InputText   = "text"
inputType InputSubmit = "submit"

data ListAction a
  = ListAppend [a]
  | ListDelete [Int]
  | ListInsert [(Int, a)]
  | ListSet [a]
  | ListNOp
  deriving (Eq, Ord, Read, Show)

data FormArrow b c where
  FormId         :: FormArrow b b
  FormApp        :: FormArrow b (c -> d) -> FormArrow b c -> FormArrow b d
  FormFun        :: (b -> c) -> FormArrow b c
  FormFirst      :: FormArrow b c -> FormArrow (b, d) (c, d)
  FormSecond     :: FormArrow b c -> FormArrow (d, b) (d, c)
  FormElem       :: Text -> [(Text, Text)]                          -> FormArrow b c -> FormArrow b c
  FormElemAttrs  :: Text -> [(Text, Text)] -> (b -> [(Text, Text)]) -> FormArrow b c -> FormArrow b c
  FormLabel      :: Maybe Text -> Text -> FormArrow b c -> FormArrow b c
  FormInput      :: InputType -> [(Text, Text)] -> Bool -> FormArrow Text Text
  FormTextArea   :: Bool -> Int -> [(Text, Text)] -> FormArrow Text Text
  FormCat        :: FormArrow c d -> FormArrow b c   -> FormArrow b d
  FormSplit      :: FormArrow b c -> FormArrow b' c' -> FormArrow (b, b') (c, c')
  FormSpan       :: Maybe Text -> Text -> FormArrow Text ()
  -- could be nice to have a Vect version which works with a fixed number of elements.
  -- the list version should allows us to add/remove elements 
  FormList       :: Text -> FormArrow (Maybe c) d -> FormArrow (ListAction c) [d]
---  FormValidator  :: (c -> IO (ValidationStatus d)) -> FormArrow (Either (ValidationStatus d) b) (Maybe c) -> FormArrow b (Maybe d)
  FormValidator  :: Validator c d -> FormArrow (Either (ValidationStatus d) b) (Maybe c) -> FormArrow b (Maybe d)
  -- it would be nice if this wasn't a wrapper around elements, because then we would validate pairs of fields that are not adjacent.
  -- But if it is not a wrapper -- how do we get access to the elements onto with we need to do addEventListener
  FormValidatorOnChange  :: Validator c d -> b -> FormArrow (Either (ValidationStatus d) b) (Maybe c) -> FormArrow b (Maybe d)
  FormValidatorOnChange'  :: Validator c d -> b -> FormArrow (Either (ValidationStatus d) b) (Maybe c) -> FormArrow (Either (ValidationStatus d) b) (Maybe d)
  FormOnClick :: b -> FormArrow b c -> FormArrow b c
--  FormList       :: [FormArrow b c] -> FormArrow [b] [c]
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
  show (FormApp f x) = "FormFun <fn> (" ++ show x ++ ")"
  show (FormFirst f) = "FormFirst (" ++ show f ++ ")"
  show (FormSecond f) = "FormSecond (" ++ show f ++ ")"
  show (FormElem nm attrs f) = "FormElem " ++ show nm ++ " " ++ show attrs ++ " (" ++ show f ++ ")"
  show (FormElemAttrs nm attrs attrsFn f) = "FormElemAttrs " ++ show attrs ++ " " ++ show nm ++ " <attrsFn> (" ++ show f ++ ")"
  show (FormLabel mCls txt f) = "FormLabel " ++ show mCls ++ " " ++ show txt ++ " (" ++ show f ++ ")"
  show (FormInput iType class_ req) = "FormInput iType=" ++ show iType ++ " class=" ++ show class_ ++ " required=" ++ show req
  show (FormTextArea req rows attrs) = "FormTextArea required = " ++ show req ++ " rows = " ++ show rows
  show (FormCat b a) = "FormCat (" ++ show b ++ ") (" ++ show a ++ ")"
  show (FormSplit b a) = "FormSplit (" ++ show b ++ ") (" ++ show a ++ ")"
  show (FormSpan iText mClass) = "FormSpan " ++ show iText ++ " " ++ show mClass
  show (FormList wrapperNm frm) = "FormSpan wrapperNm = " ++ show wrapperNm ++ " " ++ show frm
  show (FormValidator _ a) = "FormValidator <fn> " ++ show a
  show (FormValidatorOnChange _ b a) = "FormValidatorOnChange <fn>  " ++ show a
  show (FormValidatorOnChange' _ b a) = "FormValidatorOnChange <fn>  " ++ show a
  show (FormErrorRight a r) = "FormErrorRight (" ++ show a ++ ") (" ++ show r ++ ")"
  show (FormOnClick a f)   = "FormOnClick a " ++ show f
--  show (FormChoice f g)    = "FormChoice (" ++ show f ++ ") (" ++ show g ++ ")"

instance Functor (FormArrow b) where
  fmap f FormId = FormFun f
  fmap f (FormFun g) = FormFun $ f . g
  fmap f frm = FormCat (FormFun f) frm

{- This should be possible, but not worried about it yet.
-}
instance Applicative (FormArrow b) where
  pure c = FormFun (\_ -> c)
--  f <*> x = arr (uncurry (flip ($))) . first x . arr swap . first f . arr dup
  f <*> frm = FormApp f frm

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
  | GetValue -- does not perform validation
  deriving Show

{-
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

    (FormValidatorOnChange (Validator validator) vB f) ->
      do (Just listenerDiv) <- createJSElement d "div"
         appendChild parent listenerDiv
         fg <- renderForm (toJSNode listenerDiv) d f
         let validate = \(action, b) ->
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
         addEventListener listenerDiv (ev @FocusOut) (\event -> validate (Validate, vB) >> pure ()) False

         pure validate

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
-}

{-

When trying to validate a password/email fields we want do:

 1. ensure the field is not empty
 2. ensure the fields match

We can check the first field for being empty when the focus leaves. But we can not check that the fields match until the focus leaves both elements.

-- FIXME: instead of returning [JSNode] should we return a Builder/ShowS type thing?

-}


renderForm :: forall b c. JSDocument -> FormArrow b c -> IO ([JSNode], (FormAction, b) -> IO c)
renderForm d frm =
  case frm of
    FormId ->
      do let validate (action, b) = pure b
         pure $ ([], validate)

    (FormFun f) ->
      pure $ ([], \(_, b) -> pure (f b))

    (FormApp f x) ->
      do (fns, fv) <- renderForm d f
         (xns, xv) <- renderForm d x
         let validate (action, b) =
               do f' <- fv (action, b)
                  x' <- xv (action, b)
                  pure $ f' x'

         pure $ (fns ++ xns, validate)

    (FormCat f g) ->
      do (ng, gg) <- renderForm d g
         (nf, fg) <- renderForm d f
         let validate =
               (\b@(action,_) ->
                    do c <- gg b
                       fg (action, c)
               )
         pure (ng <> nf, validate)


    (FormSplit f g) ->
      do (fn, fg) <- renderForm d f
         (gn, gg) <- renderForm d g
         let validate (action, (b, b')) =
               do c  <- fg (action, b)
                  c' <- gg (action, b')
                  pure (c, c')
         pure (fn <> gn, validate)

    (FormInput iType attrs required) ->
      do (Just e) <- createJSElement d "input"
         setAttribute e "type" (inputType iType)
         when required $ setAttribute e "required" ""
         mapM_ (\(a,v) -> setAttribute e a v) attrs

--         setValue e init

         pure $ ([toJSNode e], \(action, val) ->
           case action of
             Validate ->
               do (Just v) <- fmap textFromJSString <$> getValue e
                  putStrLn $ "FormInput = " ++ show v
                  pure v
             SetValue ->
               do setValue e val
                  pure val)

    (FormTextArea required rows attrs) ->
      do (Just e) <- createJSElement d "textarea"
         when required $ setAttribute e "required" ""
         setAttribute e "rows" (Text.pack $ show rows)
         mapM_ (\(a,v) -> setAttribute e a v) attrs
--         setValue e init

         pure $ ([toJSNode e], \(action, val) ->
           case action of
             Validate ->
               do (Just v) <- fmap textFromJSString <$> getValue e
                  putStrLn $ "FormTextArea = " ++ show v
                  pure v
             SetValue ->
               do setValue e val
                  pure val)

    (FormErrorRight f er) ->
      do (fn, fg) <- renderForm d f
         (gn, eg) <- renderForm d er
         let validate (action, x) =
               case x of
                 (Left ve) ->
                   do -- putStrLn $ "FormErrorRight - " ++ show ve
                      eg (action, ve)
                      pure Nothing
                 (Right b) ->
                   do c <- fg (action, b)
                      pure (Just c)
         pure (fn <> gn, validate)

    (FormValidator (Validator validator) f) ->
      do (fn, fg) <- renderForm d f
         let validate (action, b) =
               do putStrLn $ "FormValidator"
                  mc <- fg (action, Right b)
                  case action of
                    SetValue -> pure Nothing
                    GetValue ->
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
                                    fg (action, Left $ ValidationFailure $ ValidationNone)
                                    pure $ Just d
                               ValidationFailure vf ->
                                 do putStrLn "FormValidator - validation failure"
                                    fg (action, Left $ ValidationFailure $ ValidationNone)
                                    pure $ Nothing
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
         pure (fn, validate)

    (FormValidatorOnChange (Validator validator) vB f) ->
      do -- (Just listenerDiv) <- createJSElement d "div"
--         appendChild parent listenerDiv
         (fn, fg) <- renderForm  d f
         let validate (action, b) =
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
         mapM_ (\n -> addEventListener n (ev @FocusOut) (\event -> validate (Validate, vB) >> pure ()) False) fn

         pure (fn, validate)

    (FormValidatorOnChange' (Validator validator) vB f) ->
      do -- (Just listenerDiv) <- createJSElement d "div"
--         appendChild parent listenerDiv
         (fn, fg) <- renderForm d f
         let validate (action, evb) =
              do putStrLn $ "FormValidator"
                 mc <- fg (action, evb)
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
                              ValidationFailure ValidationNone ->
                                do putStrLn "FormValidator - validation none"
--                                   fg (action, Left ve)
                                   pure $ Nothing
                              ValidationFailure vf ->
                                do putStrLn "FormValidator - validation failure"
                                   fg (action, Left ve)
                                   pure $ Nothing
         mapM_ (\n -> addEventListener n (ev @FocusOut) (\event -> validate (Validate, Right vB) >> pure ()) False) fn
         pure (fn, validate)

    FormOnClick b frm ->
      do (ns, v) <- renderForm d frm
         mapM_ (\n -> addEventListener n (ev @Click) (\event ->
                                                         do preventDefault event
                                                            stopPropagation event
                                                            putStrLn "FormOnSubmit"
                                                            v (SetValue, b) >> pure ()) False) ns
         let validate (action, val) =
               do r <- v (action, val)
                  pure r
         pure (ns, validate)

    FormSpan mClass iText ->
      do (Just e) <- createJSElement d "span"
         setTextContent e iText
  --       appendChild parent e
         case mClass of
           Nothing -> pure ()
           (Just cls) ->
             setAttribute e "class" cls
         let validate (action,t) =
               do putStrLn $ "update span contents = " ++ Text.unpack t
                  setTextContent e t
         pure ([toJSNode e], validate)

    (FormElem nm attrs f) ->
      do (Just e) <- createJSElement d nm
         mapM_ (\(a,v) -> setAttribute e a v) attrs

         (ns, r) <- renderForm d f
         mapM_ (\n -> appendChild e n) ns

--         appendChild parent e
         let validate b = do c <- r b
                             pure c
         pure ([toJSNode e], validate)

    (FormElemAttrs nm initAttrs attrsFn f) ->
      do (Just e) <- createJSElement d nm
         mapM_ (\(a,v) -> setAttribute e a v) initAttrs

         (ns, r) <- renderForm d f
         mapM_ (\n -> appendChild e n) ns

--         appendChild parent e
         let validate (action,b) =
                       do c <- r (action, b)
                          mapM_ (\(a,v) -> setAttribute e a v) (attrsFn b)
                          pure c
         pure ([toJSNode e], validate)
    (FormList wrapperNm frm) ->
      do (Just e) <- createJSElement d wrapperNm
         -- instead of this, we could have validators return a new validator. Might as well use Dunai at that point?
         validators <- newIORef []
         let validate (SetValue, listAction) =
               do case listAction of
                    (ListSet items) ->
                      do removeChildren e
                         (nodes, vs) <- unzip <$> replicateM (length items) (renderForm d frm)
                         mapM_ (appendChild (toJSNode e)) (concat nodes)
                         writeIORef validators vs
                         o <- zipWithM (\v i -> v (SetValue, i)) vs ((map Just items)  ++ (repeat Nothing))
                         pure $ o
                    (ListAppend items) ->
                      do (nodes, vs) <- unzip <$> replicateM (length items) (renderForm d frm)
                         mapM_ (appendChild (toJSNode e)) (concat nodes)
                         oldValidators <- readIORef validators
                         writeIORef validators (oldValidators ++ vs)
                         o <- zipWithM (\v i -> v (SetValue, i)) vs ((map Just items)  ++ (repeat Nothing))
                         pure $ o

             validate (Validate, items) =
               do vs <- readIORef validators
                  o <- zipWithM (\v i -> v (Validate, Nothing)) vs (repeat Nothing) -- ((map Just items)  ++ (repeat Nothing))
                  pure $ o

         pure ([toJSNode e], validate)

    _ -> error $ "renderForm: not implemented - " ++ show frm


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

nonEmptyTextV :: Validator Text Text
nonEmptyTextV = Validator nonEmptyText

fanError = (FormFun $ \x ->
      case x of
        (Left err) -> (Left err, Left err)
        (Right (x,y)) -> (Right x, Right y))

equalText :: Text -> (Text, Text) -> IO (ValidationStatus Text)
equalText errMsg (c, c')
  | Text.null c || Text.null c' = pure $ ValidationFailure ValidationNone
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

-- * nicely packaged functions

label :: Text -> FormArrow a a
label txt =
  proc a ->
    do FormElem "label" [("class", "control-label")] (FormSpan Nothing txt) -< txt
       returnA -< a

div_ :: Text -> FormArrow b c -> FormArrow b c
div_ cls frm = FormElem "div" [("class", cls)] frm

p_ :: Text -> FormArrow b c -> FormArrow b c
p_ cls frm = FormElem "p" [("class", cls)] frm

h2_ :: Text -> FormArrow b c -> FormArrow b c
h2_ cls frm = FormElem "h2" [("class", cls)] frm

li_ :: Text -> FormArrow b c -> FormArrow b c
li_ cls frm = FormElem "li" [("class", cls)] frm

fieldset_ cls frm = FormElem "fieldset" [("class",cls)] frm
