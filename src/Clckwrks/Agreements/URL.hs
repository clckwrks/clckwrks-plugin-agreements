{-# LANGUAGE DeriveDataTypeable, DataKinds, PolyKinds, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Agreements.URL where

import Clckwrks.Agreements.Types (AgreementMeta)
import Data.Data (Data, Typeable)
import Data.Proxy (Proxy(..))
-- import Data.SafeCopy               (SafeCopy(..), base, deriveSafeCopy)
import Web.Routes.TH               (derivePathInfo)


type family RequestData  (a :: k) :: *
type family ResponseData (a :: k) :: *

class KnownURL (a :: k) where knownURL :: Proxy a -> k

data AgreementsAdminApiURL
  = GetLatestAgreementsMeta
  | SetAgreements
  deriving (Eq, Ord, Data, Typeable, Read, Show)
derivePathInfo ''AgreementsAdminApiURL

-- this should be generated. Or could we some how use generics?
instance KnownURL 'GetLatestAgreementsMeta where knownURL _ = GetLatestAgreementsMeta

type instance RequestData  'GetLatestAgreementsMeta = ()
type instance ResponseData 'GetLatestAgreementsMeta = [AgreementMeta]

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

