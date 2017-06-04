{-# LANGUAGE OverloadedStrings #-}
module Lib
    (
      Client
    , ServerState
    , newServerState
    , numClients
    , clientExists
    , addClient
    , removeClient
    , broadcast
    , main
    , application
    , talk
    ) where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import qualified Data.Text as DT
import qualified Data.Text.IO as DT
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

-- Create a new, initial state
newServerState :: ServerState
newServerState = []

-- Get the number of active clients
numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client). fst)

-- クライアント全員にメッセージを送る（ついでにログ出力）
broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    DT.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    clients <- readMVar state
    case msg of
        _   | not (prefix `DT.isPrefixOf` msg) ->
                WS.sendTextData conn ("Wrong announcement" :: Text)
            | any ($ fst client)
                [DT.null, DT.any isPunctuation, DT.any isSpace] ->
                    WS.sendTextData conn ("Name cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)
            | clientExists client clients ->
                WS.sendTextData conn ("User already exists" :: Text)
            | otherwise -> flip finally disconnect $ do
                modifyMVar_ state $ \s -> do
                    let s' = addClient client s
                    WS.sendTextData conn $
                        "Welcome! Users: " `mappend`
                        DT.intercalate ", " (map fst s)
                    broadcast (fst client `mappend` " joined") s'
                    return s'
                talk conn state client
            where
                prefix = "Hi! I am "
                client = (DT.drop (DT.length prefix) msg, conn)
                disconnect = do
                    s <- modifyMVar state $ \s ->
                        let s' = removeClient client s in return (s', s')
                    broadcast (fst client `mappend` " diconnected") s

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast (user `mappend` ": " `mappend` msg)
