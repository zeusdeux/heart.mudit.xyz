{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           System.Environment            (getEnv)
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5              as H hiding (main)
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Web.Scotty                    as S
import Control.Monad (mplus)

main :: IO ()
main = do
          port <- (getEnv "PORT" `mplus` return "3000")
          S.scotty (read port :: Int) $ do
                                           S.get "/" $ do
                                                          S.html . renderHtml $ page

root :: Html
root = do
          H.head $ do
                      title "Niceties"
          body $ do
                    h1 "Hi there!"

someList :: Html
someList = do
              ul $ do
                      li "1"
                      li "2"
                      li "3"
                      li "4"

page :: Html
page = do
          root
          someList
