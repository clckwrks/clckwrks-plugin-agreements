{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, DataKinds, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TemplateHaskell, TypeFamilies, FunctionalDependencies, RankNTypes, GADTs, StandaloneDeriving, OverloadedStrings, ScopedTypeVariables, TypeApplications, KindSignatures, UndecidableInstances #-}
module Clckwrks.Agreements.URL where

import Control.Applicative((<|>))
import Clckwrks.Agreements.Types (Agreement, AgreementMeta, AgreementRevision, AgreementId(..), NewAgreementData, RevisionId(..))
import Data.Data (Data, Typeable)
import Data.Proxy (Proxy(..))
-- import Data.SafeCopy               (SafeCopy(..), base, deriveSafeCopy)
import GHC.TypeNats (KnownNat, SomeNat, Nat, natVal)
import Web.Routes                  (PathInfo(..))
import Web.Routes.PathInfo
import Web.Routes.TH               (derivePathInfo)
{-
type family RequestData  (a :: k) :: *
type family ResponseData (a :: k) :: *
-}
class KnownURL (a :: k) where
  knownURL :: Proxy a -> k
newtype AgreementNatId = MkAgreementNatId SomeNat

{-
This does not work because when we try to parse a path back to a url constructor, we would need to know the type of 'req' and 'res' in advance.

data AgreementsURL req res where
  GetLatestAgreementsMeta :: AgreementsURL () [AgreementMeta]
  CreateAgreement         :: AgreementsURL NewAgreementData AgreementRevision

-}

--  GetAgreement AgreementId ::AgreementsURL
--  GetAgreementRevision AgreementId RevisionId
--
--  SetAgreements
{-
deriving instance Show (AgreementsURL req res)
-- deriving instance (Data req, Data res) => Data (AgreementsURL req res)
deriving instance Eq (AgreementsURL req res)
deriving instance Ord (AgreementsURL req res)

data SomeAgreementsURL = forall req res. SomeAgreementsURL (AgreementsURL req res)

instance PathInfo SomeAgreementsURL where
  fromPathSegments =
     (segment "CreateAgreement" *> pure (SomeAgreementsURL CreateAgreement)) <|>
     (segment "GetLastestAgreementsMeta" *> pure (SomeAgreementsURL GetLatestAgreementsMeta))
  toPathSegments (SomeAgreementsURL CreateAgreement) = ["CreateAgreement"]
-}
-- derivePathInfo ''AgreementsURL
-- instance PathInfo (AgreementsURL req res)
{-  
 = GetAgree AgreementNatId
 | GetAgree2 AgreementId
-}
{-
agreementNatId :: AgreementId -> Proxy (MkAgreementNatId n)
agreementNatId (AgreementId n) =
  let nat = fromIntegral n
  in Proxy

-}
data AgreementsAdminApiURL
  = GetLatestAgreementsMeta
  | GetAgreement AgreementId
  | GetAgreementRevision AgreementId RevisionId
  | CreateAgreement
  | SetAgreements
  deriving (Eq, Ord, Data, Typeable, Read, Show)
derivePathInfo ''AgreementsAdminApiURL

-- mkGetAgreement :: TaggedURL AgreementsAdminApiURL GetAgreement

-- this should be generated. Or could we some how use generics?
instance KnownURL 'GetLatestAgreementsMeta where knownURL _ = GetLatestAgreementsMeta
instance KnownURL 'CreateAgreement where knownURL _ = CreateAgreement
instance KnownURL 'GetAgreement where knownURL _ = GetAgreement
instance KnownURL 'GetAgreementRevision where knownURL _ = GetAgreementRevision
{-
type instance RequestData  'GetLatestAgreementsMeta = ()
type instance ResponseData 'GetLatestAgreementsMeta = [AgreementMeta]

type instance RequestData  'CreateAgreement = NewAgreementData
type instance ResponseData 'CreateAgreement = AgreementRevision

type instance RequestData  'GetAgreement = NewAgreementData
type instance ResponseData 'GetAgreement = AgreementRevision
-}
data AgreementsAdminURL
  = AgreementsSettings
  | AgreementsSettingsJs
  | AgreementsAdminApi AgreementsAdminApiURL
  deriving (Eq, Ord, Data, Typeable, Read, Show)
derivePathInfo ''AgreementsAdminURL

data AgreementsURL
    = AgreementsAdmin AgreementsAdminURL
      deriving (Eq, Ord, Data, Typeable, Read, Show)
derivePathInfo ''AgreementsURL

data TaggedURL c (url :: *) = TaggedURL url
deriving instance (Show url) => Show (TaggedURL c url)


-- withURL :: forall url (con :: url). (KnownURL (con :: url)) => TaggedURL con url
withURL0 :: forall k (con :: k). (KnownURL (con :: k)) => TaggedURL con k
withURL0 = TaggedURL (knownURL (Proxy :: Proxy con))
{-
class (KnownURL (con :: k)) => WithURL (con :: k) where
  type TypeSig (con :: k) :: *
  withURL :: TypeSig (con :: k)

instance (KnownURL (con :: (url :: *))) => WithURL (con :: url) where
  type TypeSig (con :: url) = TaggedURL con url
  withURL = TaggedURL (knownURL (Proxy :: Proxy con))

instance (KnownURL con) => WithURL (con :: (* -> url)) where
  type TypeSig (con :: (* -> url)) = TaggedURL con url
--  withURL = TaggedURL (knownURL (Proxy :: Proxy con))
-}

class WithURL a where
  type family RequestData  (a :: k) :: *
  type family ResponseData (a :: k) :: *
  type WithURLType a :: *
  withURL :: WithURLType a

instance WithURL (GetLatestAgreementsMeta :: AgreementsAdminApiURL) where
  type RequestData  GetLatestAgreementsMeta = ()
  type ResponseData GetLatestAgreementsMeta = [AgreementMeta]
  type WithURLType GetLatestAgreementsMeta = TaggedURL GetLatestAgreementsMeta AgreementsAdminApiURL
  withURL = TaggedURL GetLatestAgreementsMeta

instance WithURL GetAgreement where
  type RequestData  GetAgreement = ()
  type ResponseData GetAgreement = Agreement
  type WithURLType GetAgreement = AgreementId -> TaggedURL GetAgreement AgreementsAdminApiURL
  withURL = \aid -> TaggedURL (GetAgreement aid)

instance WithURL CreateAgreement where
  type RequestData  CreateAgreement = NewAgreementData
  type ResponseData CreateAgreement = AgreementRevision
  type WithURLType  CreateAgreement = TaggedURL CreateAgreement AgreementsAdminApiURL
  withURL = TaggedURL CreateAgreement


{-
type family Produces con url :: Bool where
  Produces (con :: url) url = True
  Produces (con :: * -> url) url = True
-}

{-
class Produces con url where
  type CanProduce con url :: Bool

instance Produces (con :: url) (url :: *) where
  type CanProduce (con :: url) (url :: *) = True

instance Produces (a -> url) (url :: *) where
  type CanProduce (a -> url) (url :: *) = True
-}
{-
class Produces con where
  type FinalType con :: *

-- instance Produces (a :: *) where
--  type FinalType (a :: *) = a

instance Produces (a -> b) where
  type FinalType (a -> b) = FinalType b

type family FinalTypeF (con :: forall k. k)  where
  FinalTypeF (a -> b) = b
  FinalTypeF a = a
-}
{-
type family FinalType (con :: forall k. k) where
--  FinalType (a -> b) = FinalType b
  FinalType a = a
-}
{-
type family MkTagged con :: *
type instance MkTagged con = TaggedURL con (FinalType con)
-}
{-
mkTagged :: forall con url. (Proxy :: Proxy con) -> (FinalType con) -> TaggedURL con (FinalType con)
mkTagged u = TaggedURL u
-}
{-
class (FinalTypeF con ~ url) => WithCon (con :: k)  (url :: *) where
--  type family RequestData  (con :: k) :: *
--  type family ResponseData (con :: k) :: *
  type Tagged (con ) (url :: *) :: *
  type Tagged con url  = TaggedURL con url
  type WithConType con url :: *

instance WithCon GetLatestAgreementsMeta AgreementsAdminApiURL where
--  type RequestData  GetLatestAgreementsMeta = ()
--  type ResponseData GetLatestAgreementsMeta = [AgreementMeta]
--  type WithConType GetLatestAgreementsMeta AgreementsAdminApiURL = TaggedURL GetLatestAgreementsMeta AgreementsAdminApiURL
-}

{-
instance WithCon GetLatestAgreementsMeta AgreementsAdminApiURL where
--  type RequestData  GetLatestAgreementsMeta = ()
--  type ResponseData GetLatestAgreementsMeta = [AgreementMeta]
  type WithConType GetLatestAgreementsMeta AgreementsAdminApiURL = TaggedURL GetLatestAgreementsMeta AgreementsAdminApiURL
--   withURL = TaggedURL GetLatestAgreementsMeta
-}
{-
class Apply t where
  type ApplyVal t :: *
  apply :: ApplyVal t -> t

instance Apply AgreementsAdminApiURL where
  type ApplyVal AgreementsAdminApiURL = AgreementsAdminApiURL
  apply a = a

instance (Apply r) => Apply (u -> r) where
  type ApplyVal (u -> r) = (u -> ApplyVal r)
  apply f = \u -> apply (f u)

class ApplyC i t where
--  type ApplyVal t :: *
  applyC :: i -> t

instance ApplyC r r where
  applyC a = a

instance (ApplyC AgreementsAdminApiURL r) => ApplyC (Proxy GetAgreement) (AgreementId -> r) where
--  type ApplyVal (u -> r) = (u -> ApplyVal r)
  applyC f = \u -> applyC (GetAgreement u)



remote :: (ApplyC (Proxy url) t) => Proxy (url :: AgreementsAdminApiURL) -> t
remote p = applyC p
-}

-- instance ApplyC (Proxy ::Proxy CreateAgreement) AgreementsAdminApiURL
{-
instance Apply AgreementsAdminApiURL where
  type ApplyVal AgreementsAdminApiURL = AgreementsAdminApiURL
  apply a = a

instance (Apply r) => Apply (u -> r) where
  type ApplyVal (u -> r) = (u -> ApplyVal r)
  apply f = \u -> apply (f u)
-}
{-
instance (Apply c) where
  type ApplyVal c = Proxy (u :: c)
-}
{-
instance Apply AgreementsAdminApiURL AgreementsAdminApiURL where
  apply = id

instance Apply (a -> AgreementsAdminApiURL) (a -> AgreementsAdminApiURL)  where
  apply c = \a -> apply (c a)
-}

