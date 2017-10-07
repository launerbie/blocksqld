{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Blocksqld.Schema where

import qualified Data.Text as T
import Database.Persist.TH
import Data.Time (UTCTime, getCurrentTime, addUTCTime)

----------------------- Database Schema ---------------------------------
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Block
    hash T.Text
    size Int
    height Int
    version Int
    merkleroot T.Text
    time Int
    nonce Int
    bits T.Text
    difficulty Double
    chainwork T.Text
    deriving Show
|]

