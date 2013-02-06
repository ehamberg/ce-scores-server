{-# LANGUAGE OverloadedStrings #-}
module Handler.Score where

import Import
import Data.Aeson.Types
import Data.Text (pack)

getScoreR :: Handler RepJson
getScoreR = do
  scores <- runDB $ selectList [] [Desc CEScorePoints, Asc CEScoreTime]
  jsonToRepJson $ fmap entityVal scores

putScoreR :: Handler RepJson
putScoreR = do
  entry <- parseJsonBody
  case entry of
       Error s   -> jsonToRepJson $ String (pack s)
       Success v -> do
         _ <- runDB $ insert (v :: CEScore)
         jsonToRepJson $ String "ok"
