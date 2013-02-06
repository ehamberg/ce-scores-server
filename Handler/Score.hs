{-# LANGUAGE OverloadedStrings #-}
module Handler.Score where

import Import
import Data.Aeson.Types (Result (..))
import Data.Text (pack)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Convertible (convert)
import System.Posix.Types (EpochTime)
import Data.Digest.Pure.SHA (Digest, SHA1State, hmacSha1)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text.Encoding (encodeUtf8)

secret :: B.ByteString
secret = B.pack "secret" -- :-)

getScoreR :: Handler RepJson
getScoreR = do
  scores <- runDB $ selectList [] [Desc CEScorePoints, Asc CEScoreTime]
  jsonToRepJson $ fmap entityVal scores

putScoreR :: Handler RepJson
putScoreR = do
  entry <- parseJsonBody
  t <- liftIO getCurrentTime
  case entry of
       Error s   -> jsonToRepJson $ String (pack s)
       Success v -> case validate v t of
                         Left e  -> jsonToRepJson $ String e
                         Right _ -> do
                           _ <- runDB $ insert v
                           jsonToRepJson $ String "success"

validate :: CEScore -> UTCTime -> Either Text CEScore
validate s@(CEScore name timestamp score hash) t = if validHash
                                                      then Right s
                                                      else Left "Invalid hash"
  where validHash = hash == (pack . show) (scoreHash name (convert timestamp) score)

scoreHash :: Text -> EpochTime -> Int64 -> Digest SHA1State
scoreHash n t s = hmacSha1 secret scoredesc
  where scoredesc = B.fromChunks [encodeUtf8 n]
                      `B.append` B.pack "\v"
                      `B.append` (B.pack .show) t
                      `B.append` B.pack "\v"
                      `B.append` (B.pack .show) s
