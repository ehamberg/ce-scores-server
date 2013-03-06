{-# LANGUAGE OverloadedStrings #-}
module Handler.Score where

import Import
import Data.Aeson.Types (Result (..))
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Convertible (convert)
import Data.Digest.Pure.SHA (Digest, SHA1State, hmacSha1)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text.Encoding (encodeUtf8)

secret :: B.ByteString
secret = B.pack "secret" -- :-)

getScoreR :: Handler RepJson
getScoreR = do
  scores <- runDB $ selectList [] [Desc CEScorePoints, Asc CEScoreTime, LimitTo 10]
  jsonToRepJson $ fmap entityVal scores

postScoreR :: Handler RepJson
postScoreR = do
  entry <- parseJsonBody
  t <- fmap convert $ liftIO getPOSIXTime
  case entry of
       Error s   -> do
         $(logWarn) $ T.pack $ "json parsing failed: " ++ s
         jsonToRepJson $ String (T.pack s)
       Success v -> case validate v t of
                         Left e  -> do
                           $(logWarn) $ T.pack "failed validation"
                           jsonToRepJson $ String e
                         Right _ -> do
                           _ <- runDB $ insert v
                           getScoreR

validate :: CEScore -> Int -> Either Text CEScore
validate s@(CEScore name timestamp score hash) t = if validHash && validLength && validTime
                                                      then Right s
                                                      else Left "Failed validation"
  where validHash   = hash == (T.pack . show) (scoreHash name (fromIntegral timestamp) (fromIntegral score))
        validLength = T.length name < 25
        validTime   = (t-fromIntegral timestamp) < (10*24*60*60) -- 10 days

scoreHash :: Text -> Int -> Int -> Digest SHA1State
scoreHash n t s = hmacSha1 secret scoredesc
  where scoredesc = B.fromChunks [encodeUtf8 n]
                      `B.append` B.pack "\v"
                      `B.append` (B.pack .show) t
                      `B.append` B.pack "\v"
                      `B.append` (B.pack .show) s
