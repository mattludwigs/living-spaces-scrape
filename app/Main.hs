{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Maybe (listToMaybe)
import Text.HTML.TagSoup ((~==), Tag(..), parseTags)

type Url = String

urls :: [Url]
urls =
  [ "http://www.livingspaces.com/ProductView.aspx?productId=71481"
  , "http://www.livingspaces.com/Views/Mobile/productview.aspx?productId=93806"
  ]

getHtml :: String -> IO ByteString
getHtml url = do
  r <- get url
  return (r ^. responseBody)

getTagForClearance html =
  listToMaybe $ filter isClearanceTag (parseTags html)

isClearanceTag t =
  t ~== TagOpen ("div"::String) [("class","pv-clearance-flag")]

processUrl :: Url -> IO (Maybe (Tag ByteString))
processUrl url = do
  htmlForDresser <- getHtml url
  return $ getTagForClearance htmlForDresser

printMessage :: Maybe (Tag ByteString) -> IO ()
printMessage (Just _) = putStrLn "On Clearance"
printMessage Nothing = putStrLn "Not on Clearance"

main :: IO ()
main = do
  processUrl (head urls) >>= printMessage
  processUrl (last urls) >>= printMessage
