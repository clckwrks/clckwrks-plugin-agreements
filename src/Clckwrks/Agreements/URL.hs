{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Agreements.URL where

import Data.Data (Data, Typeable)
-- import Data.SafeCopy               (SafeCopy(..), base, deriveSafeCopy)
import Web.Routes.TH               (derivePathInfo)

data AgreementsAdminApiURL
  = -- GetAgreementsSettings
    GetLatestAgreementsMeta
  | SetAgreements
  deriving (Eq, Ord, Data, Typeable, Read, Show)
derivePathInfo ''AgreementsAdminApiURL

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

