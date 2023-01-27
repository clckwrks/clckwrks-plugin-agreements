{-# LANGUAGE DataKinds, DeriveDataTypeable, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Agreements.Acid
    ( module Clckwrks.Agreements.Types
      -- * state
    , AgreementsState
    , initialAgreementsState
      -- * events
    , GetAgreements(..)
    , GetLatestAgreementsMeta(..)
    , SetAgreements(..)
--    , GetAgreementsSettings(..)
    ) where

import Clckwrks.Types       (Trust(..))
import Clckwrks.Agreements.Types ( Agreement(..), AgreementMeta(..), AgreementRevision, AgreementId(..), AgreementsSettings(..), IxAgreements, Lang, RevisionId(..)
                                 , agreementId, revisionAuthor, revisionBody, revisionDate, revisionId, revisionNote
                                 )
import Clckwrks.Monad       (ThemeStyleId(..))
import Control.Applicative  ((<$>))
import Control.Arrow        (second)
import Control.Lens         ((?~), (^.), (.=), (.~), (?=), (&), assign, makeLenses, set, use, view, over)
import Control.Lens.At      (at)
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, modify, put)
import Control.Monad.Trans  (liftIO)
import Data.Acid            (AcidState, Query, Update, makeAcidic)
import Data.Data            (Data, Typeable)
import Data.Function        (on)
import           Data.IxSet.Typed ((@=))
import qualified Data.IxSet.Typed as IxSet
import Data.List            (maximumBy)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import Data.Maybe           (fromJust)
import Data.SafeCopy        (Migrate(..), base, deriveSafeCopy, extension)
import Data.String          (fromString)
import Data.Text            (Text)
import Data.Time.Clock      (UTCTime)
import qualified Data.Text  as Text
import Data.UserId          (UserId)

data AgreementsState  = AgreementsState
    { _agreements :: IxAgreements
    }
    deriving (Eq, Read, Show, Typeable)
deriveSafeCopy 1 'base ''AgreementsState
makeLenses ''AgreementsState

initialAgreementsState :: AgreementsState
initialAgreementsState =
  AgreementsState
    { _agreements     = IxSet.empty
    }

-- * events

newAgreement :: Text          -- ^ Agreement Name
             -> UTCTime       -- ^ current time
             -> UserId        -- ^ author of this revision
             -> Text          -- ^ Note
             -> Map Lang Text -- ^ agreement bodies
             -> Update AgreementsState Agreement
newAgreement nm now author note bodies =
  do as <- use agreements
     let newAgreementId =
           case IxSet.toList as of
             [] -> AgreementId 1 -- we could start at 0, but it is nice to preserve that as dummy value
             l  -> succ $ maximum (map (view agreementId) l)
         newAgreement :: Agreement
         newAgreement =
           Agreement { _agreementMeta = AgreementMeta
                                          { _amAgreementId    = newAgreementId
                                          , _amAgreementName  = nm
                                          , _amRevisionId     = RevisionId 1
                                          , _amRevisionDate   = now
                                          , _amRevisionAuthor = author
                                          , _amRevisionNote   = note
                                          }
                     , _revisionBody   = bodies
                     }
     agreements .= IxSet.insert newAgreement as
     pure newAgreement

updateAgreementBody :: AgreementId
                    -> UTCTime       -- ^ current time
                    -> UserId
                    -> Text          -- ^ Note
                    -> Map Lang Text -- ^ new agreement bodies
                    -> Update AgreementsState (Either Text Agreement)
updateAgreementBody aid now author note bodies =
  do as <- use agreements
     case IxSet.getOne (as @= aid) of
       Nothing -> pure $ Left $ "Could not find " <> (Text.pack $ show aid)
       (Just oldAgreement) ->
         do let newRevisionId = succ (oldAgreement ^. revisionId)
                updatedAgreement :: Agreement
                updatedAgreement =
                  oldAgreement & revisionId     .~ newRevisionId
                               & revisionDate   .~ now
                               & revisionAuthor .~ author
                               & revisionNote   .~ note
                               & revisionBody   .~ bodies
            agreements .= IxSet.insert updatedAgreement as
            pure (Right updatedAgreement)

getAgreements :: Query AgreementsState IxAgreements
getAgreements = view agreements

-- | Get a list of the 'AgreementMeta' for the most recent revision of each 'Agreement'.
--
-- sort order: unspecified
getLatestAgreementsMeta :: Query AgreementsState [AgreementMeta]
getLatestAgreementsMeta =
  do as <- view agreements
     let agreementsById = IxSet.groupBy as :: [(AgreementId, [Agreement])]
         groupedAgreements = map snd agreementsById :: [[Agreement]]
         latestAgreements = map (maximumBy (compare `on` view revisionId)) groupedAgreements
     pure $ map _agreementMeta latestAgreements

setAgreements :: IxAgreements -> Update AgreementsState ()
setAgreements url = agreements .= url
{-
getAgreementsSettings :: Query AgreementsState AgreementsSettings
getAgreementsSettings =
  do b <- view agreements
     pure $ AgreementsSettings b
-}
makeAcidic ''AgreementsState
  [ 'getAgreements
  , 'getLatestAgreementsMeta
  , 'setAgreements
  , 'newAgreement
  , 'updateAgreementBody
--  , 'getAgreementsSettings
  ]
