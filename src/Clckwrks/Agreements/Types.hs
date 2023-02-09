{-# language CPP, DataKinds, DeriveDataTypeable, GeneralizedNewtypeDeriving, KindSignatures, FlexibleInstances,
    MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving, TemplateHaskell #-}
module Clckwrks.Agreements.Types where

import Control.Lens         (Getter, Lens', makeLenses, to, view)
import qualified Data.ByteString as BS
import Data.Data            (Data)
import Data.Word            (Word32)
import Data.IxSet.Typed     (IxSet(..), Indexable(..), ixFun, ixList)
import Data.Map             (Map)
import qualified Data.Map   as Map
import Data.SafeCopy        (Migrate(..), base, deriveSafeCopy, extension)
import Data.Time.Clock      (UTCTime)
import Data.Text            (Text)
import Data.Typeable        (Typeable)
import Data.UserId          (UserId(..))
import Web.Routes.TH        (derivePathInfo)

agreementsPluginName :: Text
agreementsPluginName = "agreements"

type Lang = Text

-- * AgreementSettings

data AgreementsSettings = AgreementsSettings
  { _mbBaseURL           :: Maybe Text
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 1 'base ''AgreementsSettings
makeLenses ''AgreementsSettings

-- * Types which hold the Agreements

-- ** Revision

-- | the highest 'RevisionId' should be the latest version of the document.
--
-- A 'RevisionId' is unique for a specific 'AgreementId', but not unique amongst all 'Agreement'
newtype RevisionId = RevisionId { _unRevisionId :: Word32 } -- Word16 or even Word8 should be plenty, but bytes are cheap
  deriving (Eq, Ord, Read, Show, Data, Typeable, Enum)
deriveSafeCopy 1 'base ''RevisionId
derivePathInfo ''RevisionId
makeLenses ''RevisionId
{-
-- | Revision
--
-- When sorted by 'RevisionId', the highest 'RevisionId' should be the latest version of the document.
data Revision = Revision
  { _revisionId    :: RevisionId
  , _revisionDate  :: UTCTime
  , _revisionNote  :: Text -- ^ A note to administrators about why the document was revised
  , _revisionBody  :: Text -- ^ the revision as an HTML document
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 1 'base ''Revision

instance Indexable '[RevisionId] Revision where
  indices = ixList (ixFun ((:[]) . _revisionId))
-}
-- ** Agreement

newtype AgreementId = AgreementId { _unAgreementId :: Word32 } -- Word16 or even Word8 should be plenty, but bytes are cheap
  deriving (Eq, Ord, Read, Show, Data, Typeable, Enum)
deriveSafeCopy 1 'base ''AgreementId
derivePathInfo ''AgreementId
makeLenses ''AgreementId

-- | Agreement
--
-- Neither AgreementId nor RevisionId are a unique key. But
-- '(AgreementId, RevisionId)' must be unique. This is also known as
-- 'AgreementRevision'.
--
-- Instead of having a 'Map Lang Text' we could have just made each
-- 'Agreement' have a 'Lang' field and contain a single language.  But
-- that can lead to each 'Agreement' having a different 'RevisionId'
-- for different translations of the same document.  It is less messy
-- to have a single 'RevisionId' for the original+translations, and
-- any time any version of the document changes (original or
-- translated). The 'RevisionId' is updated and the user must accept
-- the changes. Occassionally this means that a user may have to
-- accept a new agreement, even though the agreement has not changed
-- in their language.
--
-- It is sensible for the 'agreementName' to be unique, but that is not a property we chose to enforce.

data AgreementMeta = AgreementMeta
  { _amAgreementId    :: AgreementId
  , _amAgreementName  :: Text  -- ^ generally things like "Privacy Policy", "Terms & Conditions"
  , _amRevisionId     :: RevisionId
  , _amRevisionDate   :: UTCTime
  , _amRevisionNote   :: Text   -- ^ A note to administrators about why the document was revised
  , _amRevisionAuthor :: UserId  -- ^ who made this change. Really, a lawyer probably 'authored' the change. We just track which admin actually put the changes into the system.
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 1 'base ''AgreementMeta


data Agreement = Agreement
  { _agreementMeta  :: AgreementMeta
  , _revisionBody   :: Map Lang Text -- ^ the revision as an HTML document
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)
deriveSafeCopy 1 'base ''Agreement
makeLenses ''Agreement
{-
agreementId    :: AgreementId
  , _agreementName  :: Text
  , _revisionId     :: RevisionId
  , _revisionDate   :: UTCTime
  , _revisionNote   :: Text
  , _revisionAuthor :: UserId
-}

agreementId :: Lens' Agreement AgreementId
agreementId f agreement = fmap (\i -> agreement { _agreementMeta = (_agreementMeta agreement) { _amAgreementId = i } } ) (f (_amAgreementId $ _agreementMeta agreement))

agreementName :: Lens' Agreement Text
agreementName f agreement = fmap (\nm -> agreement { _agreementMeta = (_agreementMeta agreement) { _amAgreementName = nm } } ) (f (_amAgreementName $ _agreementMeta agreement))

revisionId :: Lens' Agreement RevisionId
revisionId f agreement = fmap (\rid -> agreement { _agreementMeta = (_agreementMeta agreement) { _amRevisionId = rid } } ) (f (_amRevisionId $ _agreementMeta agreement))

revisionNote :: Lens' Agreement Text
revisionNote f agreement = fmap (\txt -> agreement { _agreementMeta = (_agreementMeta agreement) { _amRevisionNote = txt } } ) (f (_amRevisionNote $ _agreementMeta agreement))

revisionDate :: Lens' Agreement UTCTime
revisionDate f agreement = fmap (\utc -> agreement { _agreementMeta = (_agreementMeta agreement) { _amRevisionDate = utc } } ) (f (_amRevisionDate $ _agreementMeta agreement))

revisionAuthor :: Lens' Agreement UserId
revisionAuthor f agreement = fmap (\uid -> agreement { _agreementMeta = (_agreementMeta agreement) { _amRevisionAuthor = uid } } ) (f (_amRevisionAuthor $ _agreementMeta agreement))

type AgreementRevision = (AgreementId, RevisionId)

agreementRevision :: Getter Agreement (AgreementId, RevisionId)
agreementRevision = to $ \agreement ->
  let am = _agreementMeta agreement
  in (_amAgreementId am , _amRevisionId am)

type AgreementIxs = '[AgreementId, RevisionId, UTCTime, AgreementRevision]

instance Indexable AgreementIxs Agreement where
  indices = ixList (ixFun ((:[]) . view agreementId))
                   (ixFun ((:[]) . view revisionId))
                   (ixFun ((:[]) . view revisionDate))
                   (ixFun ((:[]) . view agreementRevision))

type IxAgreements = IxSet AgreementIxs Agreement


data NewAgreementData = NewAgreementData
  { nadAgreementName :: Text
  , nadAgreementNote :: Text
  , nadAgreementBody :: Map Lang Text
  }
  deriving (Eq, Ord, Read, Show, Data)
deriveSafeCopy 1 'base ''NewAgreementData

