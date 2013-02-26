{-# LANGUAGE TemplateHaskell #-}

module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Int (Int32)
import Data.Aeson.TH

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

$(deriveJSON id ''CEScore)
