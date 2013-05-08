-- This file is part of shortcircuit-server - URL redirect resolver
-- Copyright (C) 2013  Fraser Tweedale
--
-- shortcircuit-server is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Resolve where

import Import hiding (Request)

import qualified Control.Exception as E

import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Conduit
import Network.HTTP.Types
import Control.Monad.Trans.Resource

prepareRequest :: Request m -> Request m
prepareRequest req = req {
  method = methodHead,
  redirectCount = 0,
  checkStatus = \status hdrs cj ->
    if checkStatus' status
      then Nothing
      else Just $ E.toException $ StatusCodeException status hdrs cj
} where
  checkStatus' s = any (\f -> f s) [statusIsSuccessful, statusIsRedirection]

resolveUrl' :: String -> IO (Maybe String)
resolveUrl' url = runResourceT $ do
  req <- parseUrl url
  mgr <- liftIO $ newManager def
  rsp <- httpLbs (prepareRequest req) mgr
  return $ fmap C8.unpack $ lookup hLocation $ responseHeaders rsp

resolveUrl :: String -> IO String
resolveUrl url = do
  rslt <- resolveUrl' url
  case rslt of
    Nothing -> return url
    Just nextUrl -> resolveUrl nextUrl

resolveUrlResponse :: String -> IO (Status, String)
resolveUrlResponse url = do
    rslt <- resolveUrl url
    return (ok200, rslt)
  `E.catch` \e -> return (status e, show e) where
    status e = case e of
      InvalidUrlException _ _ -> badRequest400
      _                       -> notFound404

getResolveR :: String -> Handler RepPlain
getResolveR url = do
  (status, resolution) <- liftIO $ resolveUrlResponse url
  sendResponseStatus status $ RepPlain $ toContent resolution

  -- TODO check the cache, if we have a resolution, return it
  -- to the client.
  --
  -- If we do not recognise it, resolve it;  store the resolution,
  -- and return it to the client.
