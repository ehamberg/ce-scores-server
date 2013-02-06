{-# LANGUAGE OverloadedStrings #-}
module ScoreTest
    ( scoreSpecs
    ) where

import TestImport

scoreSpecs :: Specs
scoreSpecs =
  describe "These are some example tests" $
    it "loads the index and checks it looks right" $ do
      get_ "/scores"
      statusIs 200

      --post "/score" $ do
      --  byName "foo" "bar"

      --statusIs 200
