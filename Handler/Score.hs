{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Score where

import Import

getScoreR :: Handler RepHtml
getScoreR = defaultLayout $ do
  setTitle "Welcome To Yesod!"
  [whamlet|Hi.|]

putScoreR :: Handler RepHtml
putScoreR = undefined
