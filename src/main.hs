{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Control.Monad                 (mplus)
import           Data.Aeson
import qualified Data.ByteString.Lazy          as BSL
import           Data.Dynamic
import           Data.Map                      as M
import           System.Directory
import           System.Environment            (getEnv)
import           System.IO
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5              as H hiding (main)
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Web.Scotty                    as S

main :: IO ()
main = do
  msg <- nicetyFromKey "test"
  port <- (getEnv "PORT" `mplus` return "3000")
  S.scotty (read port :: Int) $ do
    S.get "/" $ do
      S.html . renderHtml $ page msg

root :: Html
root = do
  H.head $ do
    title "Niceties"
    body $ do
      h1 "Hi there!"

someList :: String -> Html
someList msg = do
  ol $ do
    li $ toMarkup $ msg

page :: String -> Html
page msg = do
  root
  someList msg

type Hash = Map String String
type Key = String

nicetyFromKey :: Key -> IO String
nicetyFromKey key = do
  n1 <- decodeNiceties "data/niceties.json"
  let n2 = M.lookup key <$> n1 -- lookup "test" <$> Right Hash => Right (lookup "test" Hash)
                                  -- which has type Either String (Maybe String)
  let n3 = case n2 of Left err -> err
                      Right (Just s) -> s -- m :: String
                      Right Nothing -> "Key " ++ key ++ " not found!" -- :: String
  return n3

decodeNiceties :: FilePath -> IO (Either String Hash)
decodeNiceties p = do
  rawData <- BSL.readFile p -- path relative to where process cwd
                                               -- process cwd seems to be root of project
  return $ (eitherDecode rawData :: Either String Hash)

printCwd :: IO ()
printCwd = getCurrentDirectory >>= (\p -> Prelude.putStrLn p)
