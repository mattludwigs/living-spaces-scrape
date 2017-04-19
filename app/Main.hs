{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (mapM_)
import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Maybe (listToMaybe)
import Text.HTML.TagSoup ((~==), Tag(..), parseTags)

type Url = String


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


main :: IO ()
main = do
  htmlForDresser <- getHtml "http://www.livingspaces.com/ProductView.aspx?productId=71481"

  case getTagForClearance htmlForDresser of
    Just _ ->
      putStrLn "Dresser: On clearance"
    Nothing ->
      putStrLn "Dresser: Not on clearance"

  htmlForChair <- getHtml "http://www.livingspaces.com/ProductView.aspx?productId=71481"

  case getTagForClearance htmlForChair of
    Just _ ->
      putStrLn "Chair: On clearance"
    Nothing ->
      putStrLn "Chair: Not on clearance"
