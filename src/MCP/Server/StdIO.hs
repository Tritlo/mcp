{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : MCP.Server.StdIO
Description : MCP server implementation for stdin/stdout communication
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides MCP server implementation for stdin/stdout streams.
-}
module MCP.Server.StdIO (
    -- * Server Runner
    runServer,
    ServerConfig (..),
) where

import Control.Exception (catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, put)
import Data.Aeson (decode, fromJSON, object, toJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import System.IO.Error (isEOFError)

import MCP.Protocol
import MCP.Server (MCPServer (..), MCPServerM, ServerConfig (..), ServerState (..), initialServerState, runMCPServer, sendError, sendResponse)
import MCP.Types

-- | Handle an incoming JSON-RPC message
handleMessage :: (MCPServer MCPServerM) => BSC.ByteString -> MCPServerM (Maybe ())
handleMessage input = do
    case decode (LBS.fromStrict input) :: Maybe JSONRPCMessage of
        Nothing -> do
            config <- ask
            sendError (configOutput config) (RequestId (toJSON ("unknown" :: T.Text))) $
                JSONRPCErrorInfo (-32700) "Parse error" Nothing
            return Nothing
        Just msg -> case msg of
            RequestMessage req -> do
                handleRequest req
                return (Just ())
            NotificationMessage notif -> do
                handleNotification notif
                return (Just ())
            _ -> do
                config <- ask
                sendError (configOutput config) (RequestId (toJSON ("unknown" :: T.Text))) $
                    JSONRPCErrorInfo (-32600) "Invalid Request" Nothing
                return Nothing

-- | Handle a JSON-RPC request
handleRequest :: (MCPServer MCPServerM) => JSONRPCRequest -> MCPServerM ()
handleRequest (JSONRPCRequest _ reqId method params) = do
    config <- ask
    state <- get

    case method of
        "initialize" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success initParams -> handleInitialize reqId initParams
                Aeson.Error e ->
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing ->
                sendError (configOutput config) reqId $
                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "ping" -> handlePing reqId
        "resources/list" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListResources listParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListResources (ListResourcesParams Nothing)
                        sendResponse (configOutput config) reqId result
        "resources/read" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success readParams -> do
                            result <- handleReadResource readParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        sendError (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "resources/templates/list" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListResourceTemplates listParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListResourceTemplates (ListResourceTemplatesParams Nothing)
                        sendResponse (configOutput config) reqId result
        "prompts/list" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListPrompts listParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListPrompts (ListPromptsParams Nothing)
                        sendResponse (configOutput config) reqId result
        "prompts/get" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success getParams -> do
                            result <- handleGetPrompt getParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        sendError (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "tools/list" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListTools listParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListTools (ListToolsParams Nothing)
                        sendResponse (configOutput config) reqId result
        "tools/call" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success callParams -> do
                            result <- handleCallTool callParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        sendError (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "completion/complete" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success completeParams -> do
                            result <- handleComplete completeParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        sendError (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "logging/setLevel" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success setLevelParams -> do
                    _ <- handleSetLevel setLevelParams
                    -- SetLevel response is just an empty object
                    sendResponse (configOutput config) reqId (object [])
                Aeson.Error e ->
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing ->
                sendError (configOutput config) reqId $
                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        _ ->
            sendError (configOutput config) reqId $
                JSONRPCErrorInfo (-32601) "Method not found" Nothing

handleInitialize :: RequestId -> InitializeParams -> MCPServerM ()
handleInitialize reqId params = do
    config <- ask
    state <- get

    let InitializeParams{capabilities = clientCaps} = params

    put
        state
            { serverInitialized = True
            , clientCapabilities = Just clientCaps
            , serverInfo = Just (configServerInfo config)
            }

    let result =
            InitializeResult
                { protocolVersion = "2024-11-05"
                , capabilities = serverCapabilities state
                , serverInfo = configServerInfo config
                , instructions = Nothing
                , _meta = Nothing
                }

    sendResponse (configOutput config) reqId result

handlePing :: RequestId -> MCPServerM ()
handlePing reqId = do
    config <- ask
    -- Ping response is just an empty object in MCP
    sendResponse (configOutput config) reqId (object [])

-- | Handle a JSON-RPC notification
handleNotification :: JSONRPCNotification -> MCPServerM ()
handleNotification _ = do
    return ()

-- | Run the MCP server with the given configuration
runServer :: (MCPServer MCPServerM) => ServerConfig -> IO ()
runServer config = do
    let initialState = initialServerState (configCapabilities config)

    let loop = do
            eofOrLine <-
                liftIO $
                    catch
                        (Right <$> BSC.hGetLine (configInput config))
                        (\e -> if isEOFError e then return (Left ()) else throwIO e)
            case eofOrLine of
                Left () -> return () -- EOF reached, exit gracefully
                Right line -> do
                    result <- handleMessage line
                    case result of
                        Just () -> loop
                        Nothing -> return ()

    result <- runMCPServer config initialState loop
    case result of
        Left err -> putStrLn $ "Server error: " ++ T.unpack err
        Right _ -> return () -- Don't print "Server terminated" for clean EOF
