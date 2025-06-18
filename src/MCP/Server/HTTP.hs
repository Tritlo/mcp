{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : MCP.Server.HTTP
-- Description : MCP server implementation for HTTP communication
-- Copyright   : (C) 2025 Matthias Pall Gissurarson
-- License     : MIT
-- Maintainer  : mpg@mpg.is
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides MCP server implementation for HTTP communication.
module MCP.Server.HTTP (
    -- * Server Runner
    runServerHTTP,
    HTTPServerConfig (..),
) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, put)
import Data.Aeson (encode, fromJSON, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text (Text)
import Data.Text qualified as T
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Handler, Proxy(..), Server, serve, throwError)
import Servant.API (JSON, Post, ReqBody, (:>))
import Servant.Server (err400, err500, errBody)

import MCP.Protocol
import MCP.Server (MCPServer(..), MCPServerM, ServerConfig(..), ServerState(..), runMCPServer, initialServerState)
import MCP.Types

-- | Configuration for running an MCP HTTP server
data HTTPServerConfig = HTTPServerConfig
    { httpPort :: Port
    , httpServerInfo :: Implementation
    , httpCapabilities :: ServerCapabilities
    , httpEnableLogging :: Bool
    }
    deriving (Show)

-- | MCP API definition for HTTP server (following the MCP transport spec)
type MCPAPI = "mcp" :> ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value

-- | Create a WAI Application for the MCP HTTP server
mcpApp :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> Application
mcpApp config stateVar = 
    let baseApp = serve (Proxy :: Proxy MCPAPI) (mcpServer config stateVar)
    in if httpEnableLogging config
       then logStdoutDev baseApp
       else baseApp
  where
    mcpServer :: HTTPServerConfig -> TVar ServerState -> Server MCPAPI
    mcpServer httpConfig stateTVar = handleHTTPRequest httpConfig stateTVar

-- | Handle HTTP MCP requests following the MCP transport protocol
handleHTTPRequest :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> Aeson.Value -> Handler Aeson.Value
handleHTTPRequest httpConfig stateVar requestValue = do
    -- Parse the incoming JSON-RPC message
    case fromJSON requestValue of
        Aeson.Success (msg :: JSONRPCMessage) -> do
            case msg of
                RequestMessage req -> do
                    -- Process the JSON-RPC request
                    result <- liftIO $ processHTTPRequest httpConfig stateVar req
                    case result of
                        Left err -> throwError err500 { errBody = encode $ object ["error" .= T.unpack err] }
                        Right response -> return response
                NotificationMessage notif -> do
                    -- Process notifications (no response expected)
                    _ <- liftIO $ processHTTPNotification httpConfig stateVar notif
                    return $ object [] -- Empty response for notifications
                _ -> throwError err400 { errBody = "Invalid JSON-RPC message type" }
        Aeson.Error e -> throwError err400 { errBody = LBSC.pack $ "Invalid JSON-RPC message: " ++ e }

-- | Process an HTTP MCP notification
processHTTPNotification :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> JSONRPCNotification -> IO ()
processHTTPNotification _ _ _ = do
    -- For now, just ignore notifications since they don't need responses
    -- In a more complete implementation, this would handle logging/setLevel notifications
    return ()

-- | Process an HTTP MCP request
processHTTPRequest :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> JSONRPCRequest -> IO (Either Text Aeson.Value)
processHTTPRequest httpConfig stateVar req = do
    -- Read the current state
    currentState <- atomically $ readTVar stateVar
    let dummyConfig = ServerConfig
            { configInput = undefined  -- Not used in HTTP mode
            , configOutput = undefined -- Not used in HTTP mode
            , configServerInfo = httpServerInfo httpConfig
            , configCapabilities = httpCapabilities httpConfig
            }
    
    result <- runMCPServer dummyConfig currentState (handleHTTPRequestInner req)
    case result of
        Left err -> return $ Left err
        Right (response, newState) -> do
            -- Update the state atomically
            atomically $ writeTVar stateVar newState
            return $ Right response

-- | Handle HTTP request within the MCP monad, returning proper JSON-RPC responses
handleHTTPRequestInner :: (MCPServer MCPServerM) => JSONRPCRequest -> MCPServerM Aeson.Value
handleHTTPRequestInner (JSONRPCRequest _ reqId method params) = do
    config <- ask
    state <- get
    
    case method of
        "initialize" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success initParams -> do
                    handleInitializeHTTP reqId initParams
                    let result = InitializeResult
                            { protocolVersion = "2024-11-05"
                            , capabilities = configCapabilities config
                            , serverInfo = configServerInfo config
                            , instructions = Nothing
                            , _meta = Nothing
                            }
                    return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "ping" -> return $ toJSON $ JSONRPCResponse "2.0" reqId (object [])
        "resources/list" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListResources listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListResources (ListResourcesParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "resources/read" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success readParams -> do
                            result <- handleReadResource readParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                        JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "tools/list" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListTools listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListTools (ListToolsParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "tools/call" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success callParams -> do
                            result <- handleCallTool callParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                        JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "prompts/list" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListPrompts listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListPrompts (ListPromptsParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "prompts/get" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success getParams -> do
                            result <- handleGetPrompt getParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                        JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "completion/complete" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success completeParams -> do
                            result <- handleComplete completeParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                        JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "logging/setLevel" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success setLevelParams -> do
                    _ <- handleSetLevel setLevelParams
                    return $ toJSON $ JSONRPCResponse "2.0" reqId (object [])
                Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                JSONRPCErrorInfo (-32602) "Missing params" Nothing
        _ -> return $ toJSON $ JSONRPCError "2.0" reqId $
            JSONRPCErrorInfo (-32601) "Method not found" Nothing

-- | Handle HTTP initialize request
handleInitializeHTTP :: RequestId -> InitializeParams -> MCPServerM ()
handleInitializeHTTP _ params = do
    config <- ask
    state <- get

    let InitializeParams{capabilities = clientCaps} = params

    put state
        { serverInitialized = True
        , clientCapabilities = Just clientCaps
        , serverInfo = Just (configServerInfo config)
        }

-- | Run the MCP server as an HTTP server
runServerHTTP :: (MCPServer MCPServerM) => HTTPServerConfig -> IO ()
runServerHTTP config = do
    -- Initialize the server state
    stateVar <- newTVarIO $ initialServerState (httpCapabilities config)
    putStrLn $ "Starting MCP HTTP Server on port " ++ show (httpPort config) ++ "..."
    run (httpPort config) (mcpApp config stateVar)