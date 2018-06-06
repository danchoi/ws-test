{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified Data.ByteString.Char8 as B8
import Network.WebSockets
import Network.Wai.Handler.WebSockets
import Network.Wai
import Data.Text
import qualified Data.Text.Encoding as T
import Data.Monoid
import Network.HTTP.Types
import           Data.Function                       (fix)
import           Network.Wai.UrlMap                  (UrlMap', mapUrls, mount,
                                                      mountRoot)
import           Network.Wai.Handler.Warp            (run)
import           Network.Wai.Middleware.Static       hiding ((<|>))
import Control.Concurrent


main :: IO ()
main = do
  putStrLn "hello"

  let myApp = Network.Wai.UrlMap.mapUrls ( mount "autorefresh" app )

  run 8083 $ static myApp


app :: Application
app = \req respond -> 

    case websocketsApp defaultConnectionOptions (wsApp req) req of
      Nothing -> backupApp req respond
      Just res -> do
          respond res

  where
    wsApp :: Network.Wai.Request -> ServerApp
    wsApp req pending_conn = do
        conn <- acceptRequest pending_conn
        putStrLn "Connected"
        let path :: B8.ByteString
            path = rawPathInfo req
            query = rawQueryString req
            msg = "Hello, client from " <> path <> " " <> query
        B8.putStrLn msg 
        fix $ \loop -> do
          -- poll database here for updates
          putStrLn "Sending message"
          sendTextData conn (T.decodeUtf8 msg)
          threadDelay 1000000

          loop
        

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"


